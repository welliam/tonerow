; For doing musical serialism permutations upon ordered sets of objects (e.g.,
; tone rows)

; "Parent rows" are ordered rows which compose the members of "child rows"
; (usually just called "rows"). For example, the twelve tone row as represented
; By the numbers 0..11 would be defined as the a parent row
; `(0 1 2 3 4 5 6 7 8 9 10 11)', a list.
; if we wanted 0 to be c, then we would could define this as the list
; (C Db D [..] B). Child rows are derived as members of these lists, so
; '(0 2 4 6 8 10 1 3 5 7 9 11), '(0 1 2), and '(0 0 0) might all be child rows.
; Note that they needn't necessarily be the same length as the parent row, and
; they may contain duplicate members. Parent rows must be composed entirely of
; unique members.

(load "prelude.scm") ; some odds and ends

(define/protect (index-of (x : (cut member <> lst)) (lst : list?))
  ; for finding the index of an object in a row
  (if (equal? x (car lst))
      0
      (+ 1 (index-of x (cdr lst)))))

(define/protect (sublist? (lst1 : list?) (lst2 : list?))
  (or (null? lst1)
      (and (member (car lst1) lst2)
           (sublist? (cdr lst1) lst2))))

; turning a row to and from numbers is useful as a momentary representation
; we use it for transposition and inversion, which both require adding or
; subtracting the positions of individual members
(define/protect (row->numbers
                  (row   : list? (cut sublist? <> p-row))
                  (p-row : list?))
  (map (lambda (x) (index-of x p-row)) row))

(define/protect (numbers->row
                  (numbers : (list-of integer?))
                  (p-row   : list?))
  (map (lambda (x)
         (cond
          ((negative? x)
           (list-ref (reverse p-row) (- (* x -1) 1)))
          ((>= x (length p-row))
           (list-ref p-row (- x (length p-row))))
          (else (list-ref p-row x))))
       numbers))


; --- Transposition -------------------------------
(define/protect (transpose-to (row : list?) (p-row : list?) (new : id))
  ; new should be a member of the p-row
  (transpose row p-row
             (- (index-of new p-row)
                (index-of (car row) p-row))))

(define/protect (transpose (row   : list?)
                           (p-row : list?)
                           (n     : integer?))
  (numbers->row (map (lambda (x) (+ x n))
                     (row->numbers row p-row))
                p-row))


; --- Permutations --------------------------------
; --- retrograde
(define/protect (retrograde (x : list?))
  (reverse x))

; --- inversion
(define/protect (invert-numbers (numbers : (list-of integer?)))
  (let ((head (car numbers)))
    (map (lambda (x) (+ (* (- x head) -1) head))
         numbers)))

(define/protect (invert (row : list? (cut sublist? <> p-row)) (p-row : list?))
  (numbers->row (invert-numbers (row->numbers row p-row))
                p-row))

; --- permutation compositions
(define/protect (retrograde-invert
                  (row   : list? (cut sublist? <> p-row))
                  (p-row : list?))
  (retrograde (invert row p-row)))

(define/protect (invert-retrograde
                  (row   : list? (cut sublist? <> p-row))
                  (p-row : list?))
  (invert (retrograde row) p-row))

; --- matrix
(define/protect (matrix
                  (row   : list? (cut sublist? <> p-row))
                  (p-row : list?))
  (map (lambda (x) (transpose-to row p-row x))
       (invert row p-row)))

; --- printing
(define/protect (print-permutations
                  (row   : list? (cut sublist? <> p-row))
                  (p-row : list?))
  (for-each (lambda (x)
              (for-each display `(,(car x) ":\t" ,(car (cdr x)) "\n")))
            `((P  ,row)
              (R  ,(retrograde row))
              (I  ,(invert row p-row))
              (RI ,(retrograde-invert row p-row))
              (IR ,(invert-retrograde row p-row))))
  (newline))

; --- row translations
(define/protect (translate-row
                  (row  : list? (cut sublist? <> from))
                  (from : list?)
                  (to   : list?))
  ; translates row `row' from parents `from' to 'to'
  (map (lambda (x) (list-ref to (index-of x from)))
       row))

; --- Some useful tone rows -----------------------
(define dynamics-row '(pp p mp mf f ff))
(define 12-tone-row (range 12))
(define 12-tone-row:c '(C Db D Eb E F Gb G Ab A Bb B))
(define 24-tone-row:c
  '(C Db- Db D- D Eb- Eb E- E F- F Gb- Gb G- G Ab- Ab A- A Bb- Bb B- B C-))



; --- Examples ------------------------------------
(define (examples)
  (invert '(pp mp f p mf ff) dynamics-row) ; -> '(pp f mp ff mf p)
  (invert '(p mf mp) dynamics-row)         ; -> '(p ff pp)
  (print-permutations '(p mf mp) dynamics-row)
  ; -> prints out
  ; P:	(p mf mp)
  ; I:	(p ff pp)
  ; R:	(mp mf p)
  ; RI:	(pp ff p)
  ; IR:	(mp p mf)
  (for-each displayln (matrix '(p mf mp f) dynamics-row))
  (newline)

  (define our-row '(0 2 4 6 8 10 1 3 5 7 8 11))

  (retrograde our-row)         ; -> '(11 8 7 5 3 1 10 8 6 4 2 0)
  (invert our-row 12-tone-row) ; -> '(0 10 8 6 4 2 11 9 7 5 4 1)

  (define berg-violin-concerto-row '(G Bb D Gb A C E Ab B Db Eb F))
  (print-permutations berg-violin-concerto-row 12-tone-row:c)
  ; -> prints out
  ; P:	(G Bb D Gb A C E Ab B Db Eb F)
  ; I:	(G E C Ab F D Bb Gb Eb Db B A)
  ; R:	(F Eb Db B Ab E C A Gb D Bb G)
  ; RI:	(A B Db Eb Gb Bb D F Ab C E G)
  ; IR:	(F G A B D Gb Bb Db E Ab C Eb)

  (transpose berg-violin-concerto-row 12-tone-row:c 5)
  ; -> '(C Eb G B D F A Db E Gb Ab Bb)

  (define in-memorium '(0 11 8 9 10)) ; in memorium dylan thomas

  (print-permutations in-memorium 12-tone-row)
  ; -> prints out
  ; P:	(0 11 8 9 10)
  ; I:	(0 1 4 3 2)
  ; R:	(10 9 8 11 0)
  ; RI:	(2 3 4 1 0)
  ; IR:	(10 11 0 9 8)

  (print "-------------------------")
  (print-permutations '(C D E F G A B) 12-tone-row:c)

  (print "Berg Violin Concerto Matrix:")
  (for-each displayln (matrix berg-violin-concerto-row 12-tone-row:c))
  (newline)
  (print "-------------------------")
  (print "In Memorium Matrix:")
  (for-each displayln (matrix in-memorium 12-tone-row)))

; (examples)

(define (print-12:c-matrix x)
  (for-each displayln (cdr (matrix x 12-tone-row:c))))

(define (main)
  (print-12:c-matrix (read))
  (newline)
  (main))

; --- MODES ---------------------------------------
(define/protect (sort-scale
                 (scale  : list? (cut sublist? <> pscale))
                 (pscale : list?))
  (sort scale (lambda (x y) (member y (member x pscale)))))

(define/protect (shift-list (lst : nonempty-list?) )
  (append (cdr lst) (list (car lst))))

(define/protect (find-modes/untransposed (scale : list?))
  (let loop ((s scale) (n (length scale)))
    (if (zero? n)
        '()
        (cons s (loop (shift-list s) (- n 1))))))

(define/protect (find-modes
                  (scale  : list? (cut sublist? <> parent))
                  (parent : list?))
  (map (lambda (s)
         (sort-scale (transpose-to s parent (car scale))
                     parent))
       (find-modes/untransposed scale)))

(define find-modes/c (cut find-modes <> 12-tone-row:c))

(define/protect (mode-of-limited-transposition?
                  (scale  : list? (cut sublist? <> parent))
                  (parent : list?))
  (member* scale (cdr (find-modes scale parent)) list=?))

(define molt? mode-of-limited-transposition?)

(define molt?/c (cut mode-of-limited-transposition? <> 12-tone-row:c))

; --- INTERPOLATION -------------------------------
(define/protect (interpolate
                  (scale  : list? (cut sublist? <> pscale))
                  (pscale : list?)
                  (n      : number? (cut <= <> (length scale))))
  (let ((interpol
         (fold-right1 (lambda (note rest)
                        (append
                         (if (negative? n)
                             (append (transpose (list note) pscale n)
                                     (list note))
                             (cons note
                                   (transpose (list note) pscale n)))
                         rest))
                      scale)))
    (remove-duplicates
     (if (negative? n)
         (append (cdr interpol) (list (car interpol)))
         interpol))))

(define interpolate/c (cut interpolate <> 12-tone-row:c <>))

; (define (symmetrical-scale? scale pscale)
;   (let ((ints (intervals scale pscale))
;         (rev-ints (intervals (reverse scale) pscale)))
;     (equal? ints (map abs rev-ints))))

(define (intervals row prow)
  (let ((new (translate-row row prow (range (length prow)))))
    (map - (cdr new) new)))

(define (unintervals intervals prow)
  (cons (car prow)
        (map (cut list-ref prow <>)
             (fold-right1 (lambda (a lst)
                            (cons a (map (cut + a <>) lst)))
                          intervals))))
