; unbalancing binary tree implementation
; also provides a sort and duplicate removal function

(load "prelude.scm")

(define-syntax synonym
  ; slightly better than just define for error messages
  (syntax-rules ()
    ((_ old new)
     (define (new . xs) (apply old xs)))))

(synonym (compose car cdr)    second)
(synonym (compose second cdr) third)
(synonym car                  binary-value)
(synonym second               binary-branch-less)
(synonym third                binary-branch-more)

(define empty-tree (string-copy "{}"))

(define (binary-null? x)
  (eq? x empty-tree))

(define (binary-set-less new tree)
  (list (binary-value tree)
        new
        (binary-branch-more tree)))

(define (binary-set-more new tree)
  (list (binary-value tree)
        (binary-branch-less tree)
        new))

(define (binary-cons x tree compare)
  (let ((new (binary-add x tree compare)))
    (if (eq? new 'same) tree new)))

(define (binary-cons* tree compare . xs)
  (fold-left (lambda (a tree) (binary-cons a tree compare))
             tree
             xs))

(define (binary-add x tree compare)
  ; returns 'same if x was in tree, else adds it and returns the new tree
  (if (binary-null? tree)
      (list x empty-tree empty-tree)
      (case (compare x (binary-value tree))
        ((less)
         (binary-set-less
          (binary-cons x (binary-branch-less tree) compare)
          tree))
        ((more) (binary-set-more
                 (binary-cons x (binary-branch-more tree) compare)
                 tree))
        (else 'same))))

(define (tree->list tree)
  (if (binary-null? tree)
      '()
      (append (tree->list (binary-branch-less tree))
              (list (binary-value tree))
              (tree->list (binary-branch-more tree)))))

(define (list->binary-tree lst compare)
  (fold-left (lambda (x tree)
               (binary-cons x tree compare))
             empty-tree
             lst))

(define (binary-tree compare . xs)
  (list->binary-tree xs compare))

(define (binary-tree-exists? x tree compare)
  (eq? (binary-add x tree compare) 'same))

(define (binary-count-nodes tree)
  (if (binary-null? tree)
      0
      (+ 1
         (binary-count-nodes (binary-branch-less tree))
         (binary-count-nodes (binary-branch-more tree)))))

(define-syntax define-binary-type
  (syntax-rules (make: from-list: cons: add: exists?:)
    ((_ compare) #t)
    ((_ compare make: name . rest)
     (begin
       (define (name . xs) (apply binary-tree compare xs))
       (define-binary-type compare . rest)))
    ((_ compare from-list: name . rest)
     (begin
       (define (name lst) (list->binary-tree lst compare))
       (define-binary-type compare . rest)))
    ((_ compare cons: name . rest)
     (begin
       (define (name x t) (binary-cons x t compare))
       (define-binary-type compare . rest)))
    ((_ compare add: name . rest)
     (begin
       (define (name x t) (binary-add x t compare))
       (define-binary-type compare . rest)))
    ((_ compare exists?: name . rest)
     (begin
       (define (name x t) (binary-tree-exists? x t compare))
       (define-binary-type compare . rest)))
    ((_ compare cons*: name . rest)
     (begin
       (define (name t . xs) (apply binary-cons* t compare xs))
       (define-binary-type compare . rest)))))

(define (make-comparator less-than equal)
  (lambda (x y)
    (cond ((less-than x y) 'less)
          ((equal x y) 'equal)
          (else 'more))))

; numbers
(define-binary-type (make-comparator < =)
  make:      number-tree
  from-list: list->number-tree
  cons:      number-tree-cons
  exists?:   number-tree-exists?)

; strings
(define-binary-type (make-comparator string<? string=?)
  make:      string-tree
  from-list: list->string-tree
  cons:      string-tree-cons
  exists?:   string-tree-exists?)

; numlists
(define (compare-numlist t1 t2)
  (let loop ((t1 t1) (t2 t2) (comparison-so-far 'equal))
    (cond
     ((and (null? t1) (null? t2)) comparison-so-far)
     ((null? t1) 'less)
     ((null? t2) 'more)
     ((eq? comparison-so-far 'equal)
      (loop (cdr t1) (cdr t2)
            (cond
             ((< (car t1) (car t2)) 'less)
             ((> (car t1) (car t2)) 'more)
             (else 'equal))))
     (else (loop (cdr t1) (cdr t2) comparison-so-far)))))

(define-binary-type compare-numlist
  make:      numlist-tree
  from-list: list->numlist-tree
  add:       numlist-tree-add
  cons:      numlist-tree-cons
  cons*:     numlist-tree-cons*
  exists?:   numlist-tree-exists?)

(define (random-numlist)
  (if (zero? (random 5))
      (list (random 100))
      (cons (random 100) (random-numlist))))

(define (make-list n thunk)
  (if (zero? n)
      (list (thunk))
      (cons (thunk) (make-list (- n 1) thunk))))

(define (sort lst less-than?)
  (tree->list (list->binary-tree lst (make-comparator less-than? equal?))))

(define remove-duplicates/lists
  (compose tree->list list->numlist-tree))
