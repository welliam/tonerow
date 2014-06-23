; For doing musical serialism permutations upon ordered sets of objects (e.g.,
; tone rows)

; "Parent rows" are ordered rows which compose the members of "child rows"
; (usually just called "rows"). For example, the twelve tone row as represented
; By the numbers 0..1 would be defined as the a parent row
; `(0 1 2 3 4 5 6 7 8 9 10 11)', a list.
; if we wanted 0 to be c, then we would could define this as the list
; (C Db D [..] B). Child rows are derived as members of these lists, so
; '(0 2 4 6 8 10 1 3 5 7 9 11), '(0 1 2), and '(0 0 0) might all be child rows.
; Note that they needn't necessarily be the same length as the parent row, and
; they may contain duplicate members. Parent rows must be composed entirely of
; unique members.

;** TODO: - add more error clauses to make code safer

(load "prelude.scm") ; some odds and ends

(define (index-of x lst)
  ; for finding the index of an object in a row
  (cond
   ((null? lst) (error "index-of -- not found in list: " x))
   ((equal? x (car lst)) 0)
   (else (+ 1 (index-of x (cdr lst))))))

; turning a row to and from numbers is useful as a momentary representation
; we use it for transposition and inversion, which both require adding or
; subtracting the positions of individual members
(define (row->numbers row p-row)
  (map (lambda (x) (index-of x p-row)) row))

(define (numbers->row numbers p-row)
  (map (lambda (x)
         (cond
          ((negative? x)
           (list-ref (reverse p-row) (- (* x -1) 1)))
          ((>= x (length p-row))
           (list-ref p-row (- x (length p-row))))
          (else (list-ref p-row x))))
       numbers))


; --- Transposition -------------------------------
(define (transpose-to row p-row new)
  ; new should be a member of the p-row
  (transpose row p-row (- (index-of new p-row)
                          (index-of (car row) p-row))))

(define (transpose row p-row n)
  (numbers->row (map (lambda (x) (+ x n))
                     (row->numbers row p-row))
                p-row))


; --- Permutations --------------------------------
; --- retrograde
(define (retrograde x) ; the easiest sort of permutation to implement
  (reverse x))

; --- inversion
(define (invert-numbers numbers)
  (let ((head (car numbers)))
    (map (lambda (x) (+ (* (- x head) -1) head))
         numbers)))

(define (invert row parent-row)
  (numbers->row (invert-numbers (row->numbers row parent-row))
                parent-row))

; --- permutation compositions
(define (retrograde-invert row p-row)
  (retrograde (invert row p-row)))

(define (invert-retrograde row p-row)
  (invert (retrograde row) p-row))

; --- matrix
(define (matrix row p-row)
  (map (lambda (x) (transpose-to row p-row x))
       (invert row p-row)))

; --- printing
(define (print-permutations row p-row)
  (for-each (lambda (x)
              (for-each display `(,(car x) ":\t" ,(car (cdr x)) "\n")))
            `((P  ,row)
              (R  ,(retrograde row))
              (I  ,(invert row p-row))
              (RI ,(retrograde-invert row p-row))
              (IR ,(invert-retrograde row p-row))))
  (newline))

; --- row translations
(define (translate-row row from to)
  ; translates row `row' from parents `from' to 'to'
  (map (lambda (x) (list-ref to (index-of x from)))
       row))

; --- Some useful tone rows -----------------------
(define dynamics-row '(pp p mp mf f ff))
(define 12-tone-row (range 12))
(define 12-tone-row:c '(C Db D Eb E F Gb G Ab A Bb B))


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

  (print-permutations '(0 11 8 9 10) 12-tone-row) ; in memorium dylan thomas
  ; -> prints out
  ; P:	(0 11 8 9 10)
  ; I:	(0 1 4 3 2)
  ; R:	(10 9 8 11 0)
  ; RI:	(2 3 4 1 0)
  ; IR:	(10 11 0 9 8)

  (printf "-------------------------~%")
  (print-permutations '(C D E F G A B) 12-tone-row:c))
