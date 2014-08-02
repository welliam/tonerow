(load "tonerow.scm")


; note rotation dot tumblr dot com
(define *finished-rotation* "finished")

(define (succ x lst)
  (let loop ((t lst))
    (cond
     ((and (null? (cdr t)) (eq? x (car t)))
      *finished-rotation*)
     ((eq? x (car t))
      (second t))
     (else (loop (cdr t))))))

(define (rotate-scale scale pscale)
  (map (cut succ <> pscale) scale))

(define (all-rotations scale pscale)
  (let loop ((s scale))
    (if (memq *finished-rotation* s)
        '()
        (cons s (loop (rotate-scale s pscale))))))

(define (combine-rotations rotations)
  (apply append (apply map list rotations)))


; bits
(define (bit-succ bit)
  (cond
   ((null? bit) '(1))
   ((= (car bit) 0) (cons 1 (cdr bit)))
   (else (cons 0 (bit-succ (cdr bit))))))

(define (bit-range n)
  (let loop ((x '()) (i 0) (acc '()))
    (if (= i n)
        acc
        (loop (bit-succ x)
              (add1 i)
              (cons (cons 1 x) acc)))))


; primes
(define (prime? n)
  (let loop ((i 2))
    (or (= i n)
        (and (not (zero? (modulo n i)))
             (loop (+ i 1))))))

(define (prime-factors n)
    (filter (lambda (i) (and (prime? i) (zero? (modulo n i))))
            (map (cut + 2 <>)
                 (range (/ n 2)))))

(define (take-every lst n)
  (let loop ((lst lst) (i 1))
    (cond
     ((null? lst) '())
     ((= i 1) (cons (car lst) (loop (cdr lst) n)))
     (else (loop (cdr lst) (- i 1))))))

(define (divide-list lst n)
  (take-every lst (/ (length lst) n)))

(define (prime-symmetrical-scales n)
  (let loop ((ns (range n))
             (primes (prime-factors n)))
    (map (cut divide-list ns <>) primes)))


; molts
(define (encode-bit-list bit-list t)
  (cond 
   ((null? bit-list) '())
   ((zero? (car bit-list))
    (encode-bit-list (cdr bit-list) (cdr t)))
   (else (cons (car t)
               (encode-bit-list (cdr bit-list) (cdr t))))))

(define (every-combination t)
  (map (lambda (bit-list)
         (encode-bit-list bit-list t))
       (bit-range (expt 2 (- (length t) 1)))))

(define (every-interpolation interval pscale)
  (let ((rotations (all-rotations interval pscale)))
    (map combine-rotations 
         (every-combination rotations))))

(define (molts n)
  (let ((pscale (range n)))
    (remove-duplicates/lists
     (apply append
            (map (lambda (prime)
                   (every-interpolation prime pscale))
                 (prime-symmetrical-scales n))))))


; finding "pure" or "true" molts
(define (sublist? x y)
  (cond 
   ((null? x) #t)
   ((memq (car x) y)
    (sublist? (cdr x) y))
   (else #f)))

(define (count-sublists lst1 lsts)
  (fold-left (lambda (lst2 count)
               (if (sublist? lst1 lst2)
                   (add1 count)
                   count))
             0 lsts))

(define (pure-molts molt-list)
  (filter (lambda (t)
            (= (count-sublists t molt-list) 2))
          molt-list))


; printing
(define (print-molts n)
  (let ((molts (sort (molts n) (lambda (a b) (< (length a) (length b))))))
    (print "** " n " tone scale\n")
    (print "** prime symmetrical scales")
    (for-each print (prime-symmetrical-scales n))
    (print "\n** molts")
    (for-each print molts)
    (print "\n** pure molts")
    (for-each print (pure-molts molts))))

(define (print-molt-to-file n)
  (with-output-to-file 
      (if (< n 10)
          (format "scales/molts0~a.txt" n)
          (format "scales/molts~a.txt" n))
    (cut print-molts n))
  (print "finished" n))

(define (print-all-molts-to-file)
  (for-each print-molt-to-file
            (map (cut + 2 <>) 
                 (range 23))))

(define (print-translated-molts n pscale)
  (let* ((translate-to-c 
          (lambda (scales)
            (map (cut translate-row <> (range n) pscale) scales)))
         (molts (sort (molts n) (lambda (a b) (< (length a) (length b)))))
         (primes (translate-to-c (prime-symmetrical-scales n)))
         (pures (translate-to-c (pure-molts molts)))
         (molts (translate-to-c molts)))
    (print "** " n " tone scale\n")
    (print "** prime symmetrical scales")
    (for-each print primes)
    (print "\n** molts")
    (for-each print molts)
    (print "\n** pure molts")
    (for-each print pures)))