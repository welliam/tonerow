;- a normal cut -----------------------------------
(define-syntax cut
  (syntax-rules ()
    ((_ . xs)
     (cut-help () () xs))))

(define-syntax cut-help
  (syntax-rules (<>)
    ((_ ks b ()) (lambda ks b))
    ((_ (ks ...) (b ...) (<> . rest))
     (cut-help (ks ... x) (b ... x) rest))
    ((_ (ks ...) (b ...) (x . rest))
     (cut-help (ks ...) (b ... x) rest))))

;- various things ---------------------------------
(define (displayln x) (display x) (newline))
(define (print . xs) (for-each display xs) (newline))
(define call/cc call-with-current-continuation)

(define (fold-right f x lst)
  (if (null? lst)
      x
      (f (car lst) (fold-right f x (cdr lst)))))

(define fold-right1 (cut fold-right <> '() <>))

(define (filter p lst)
  (fold-right1 (lambda (a d)
                 (if (p a) (cons a d) d))
               lst))

;- membership tests -------------------------------
(define (member* x lst p)
  (cond
   ((null? lst) #f)
   ((p x (car lst)) lst)
   (else (member* x (cdr lst) p))))

(define (list=? lst1 lst2)
  (cond
   ((and (null? lst1) (null? lst2)) #t)
   ((or (null? lst1) (null? lst2)) #f)
   (else (and (equal? (car lst1) (car lst2))
              (list=? (cdr lst1) (cdr lst2))))))

(define (remove-duplicates lst)
  (let loop ((lst lst) (found '()))
    (cond
     ((null? lst) (reverse found))
     ((member (car lst) found)
      (loop (cdr lst) found))
     (else (loop (cdr lst) (cons (car lst) found))))))

(define (remove-duplicates/lists lst)
  (let loop ((lst lst) (found '()))
    (cond
     ((null? lst) (reverse found))
     ((member* (car lst) found list=?)
      (loop (cdr lst) found))
     (else (loop (cdr lst) (cons (car lst) found))))))

;- protect ----------------------------------------

(define (compose-predicates . predicates)
  (lambda (x)
    (or (null? predicates)
        (and ((car predicates) x)
             ((apply compose-predicates (cdr predicates)) x)))))

(define-syntax define/protect
  (syntax-rules ()
    ((_ (name args ...) body ...)
     (define name
       (lambda/protect (args ...) body ...)))))

(define-syntax lambda/protect
  (syntax-rules (:)
    ((_ ((arg : . ps) ...) body ...)
     (lambda (arg ...)
       (if (and ((apply compose-predicates (list . ps)) arg) ...)
           (begin body ...)
           (error
            "Bad argument given to function. Arguments and predicates:\n"
            `((,arg . ps) ...)))))))

(define/protect (not-p (p : procedure?))
  (lambda xs
    (not (apply p xs))))

(define/protect (list-of (p : procedure?))
  (lambda (lst)
    (and (list? lst)
         (or (null? lst)
             (and (p (car lst))
                  ((list-of p) (cdr lst)))))))

(define/protect (range (n : number? (not-p negative?)))
  (let loop ((i 0))
    (if (>= i n)
        '()
        (cons i (loop (+ i 1))))))

(define/protect (nonempty-list? (x : yes))
  (and (list? x) (not (null? x))))

(define (yes x) #t)

(define (id x) x)

;- cut* -------------------------------------------

(define-syntax cut*
  (syntax-rules ()
    ((_ . xs)
     (inner-cut* () () xs rest-arg #f (finish-cut*   )))))

(define-syntax apply-syn-cont
  (syntax-rules ()
    ((_ (op arg ...) value ...)
     (op arg ... value ...))))

(define-syntax finish-cut*
  (syntax-rules ()
    ((_ formals body rest-arg #t)
     (add-rest-arg () formals rest-arg body))
    ((_ formals body rest-arg #f)
     (lambda formals body))))

(define-syntax add-rest-arg
  (syntax-rules ()
    ((_ (formals ...) () rest-arg body)
     (lambda (formals ... . rest-arg) body))
    ((_ (new-formals ...) (old-formal . rest) rest-arg body)
     (add-rest-arg (new-formals ... old-formal) rest rest-arg body))))

(define-syntax inner-cut*
  (syntax-rules (cut* cut)
    ((_ formals body () rest-arg rest-arg? k)
     (apply-syn-cont k formals body rest-arg rest-arg?))
    ((_ formals body ((cut* x ...) . tail) rest-arg rest-arg? k)
     (inner-cut* formals body tail rest-arg rest-arg?
       ; avoid problems with this cut* collecting slots from embedded cut*s
       (cut-atom-recur (cut* x ...) k   )))
    ((_ formals body ((cut x ...) . tail) rest-arg rest-arg? k)
     (inner-cut* formals body tail rest-arg rest-arg?
       ; avoid problems with this cut* collecting slots from embedded cuts
       (cut-atom-recur (cut x ...) k   )))
    ((_ formals body ((head ...) . tail) rest-arg rest-arg? k)
     (inner-cut* () () tail rest-arg rest-arg?
       (cut-list-recur formals body (head ...) k   )))
    ((_ formals body (head . tail) rest-arg rest-arg? k)
     (inner-cut* formals body tail rest-arg rest-arg?
       (cut-atom-recur head k  )))))

(define-syntax cut-list-recur
  (syntax-rules ()
    ((_ old-formals old-body
        head-body k
        tail-formals tail-body
        rest-arg rest-arg?)
     (inner-cut* old-formals old-body head-body rest-arg rest-arg?
       (cut-list-recur2 tail-formals tail-body k   )))))

(define-syntax cut-list-recur2
  (syntax-rules ()
    ((_ (tail-formal ...) (tail-body ...)
        k
        (head-formal ...) (head-body ...)
        rest-arg rest-arg?)
     (apply-syn-cont k
       (head-formal ... tail-formal ...)
       ((head-body ...) tail-body ...)
       rest-arg rest-arg?))))

(define-syntax cut-atom-recur
  (syntax-rules (<> <...>)
    ((_ <...> k (formal ...) (body ...) rest-arg rest-arg?)
     (apply-syn-cont k (formal ...) (rest-arg body ...) rest-arg #t))
    ((_ <> k (formal ...) (body ...) rest-arg rest-arg?)
     (apply-syn-cont k (x formal ...) (x body ...) rest-arg rest-arg?))
    ((_ x k formals (body ...) rest-arg rest-arg?)
     (apply-syn-cont k formals (x body ...) rest-arg rest-arg?))))

;- union types ------------------------------------

(define (any-predicate ps x)
  (let loop ((ps ps))
    (and (not (null? ps))
         (or ((car ps) x)
             (loop (cdr ps))))))

(define (union-predicate . predicates)
  (lambda (x)
    (any-predicate predicates x)))

;- pointless --------------------------------------
(define (partial-apply f . given-args)
  (lambda restof-args
    (apply f (append given-args restof-args))))

(define (compose f g)
  (lambda args
    (f (apply g args))))

(define-syntax pointless
  (syntax-rules ()
    ((_ (f args ...))
     (partial-apply f args ...))
    ((_ f) f)
    ((_ (f args ...) . rest)
     (compose (partial-apply f args ...)
              (pointless . rest)))
    ((_ f . rest)
     (compose f (pointless . rest)))))
