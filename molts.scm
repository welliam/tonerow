(load "tonerow.scm") ; we need translate-row
(load "binary-tree.scm")


;- note rotation ----------------------------------
(define *finished-rotation* (string-copy "finished"))

(define (succ x lst)
  (let* ((search (memq x lst)) (tail (cdr search)))
    (if (null? tail)
        *finished-rotation*
        (car tail))))

(define (rotate-scale scale pscale)
  (map (cut succ <> pscale) scale))

(define (all-rotations scale pscale)
  (let loop ((s scale))
    (if (memq *finished-rotation* s)
        '()
        (cons s (loop (rotate-scale s pscale))))))

(define (combine-rotations rotations)
  (apply append (apply map list rotations)))


;- bits -------------------------------------------
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


;- primes -----------------------------------------
(define (prime? n)
  (and (not (= n 1))
       (let loop ((i 2))
         (or (= i n)
             (and (not (zero? (modulo n i)))
                  (loop (+ i 1)))))))

(define (prime-factors n)
    (filter (lambda (i) (and (zero? (modulo n i)) (prime? i)))
            (map (cut + 2 <>)
                 (range (/ n 2)))))

(define (good-primes limit)
   (filter (lambda (n)
             (let ((facs (prime-factors n)))
               (and (= (length facs) 1)
                    (not (any (lambda (n) (memq n facs))
                              (range 6))))))
           (range limit)))

(define (count-molts n)
  (length (molts n)))

(define (take-every lst n)
  (map car
       (filter (lambda (p) (zero? (modulo (second p) n)))
               (zip-range lst))))

(define (divide-list lst n)
  (take-every lst (/ (length lst) n)))

(define (prime-symmetrical-scales n)
  (let loop ((ns (range n))
             (primes (prime-factors n)))
    (map (cut divide-list ns <>) primes)))


;- molts ------------------------------------------
(define (encode-bit-list bit-list t)
  (cond
   ((null? bit-list) '())
   ((zero? (car bit-list))
    (encode-bit-list (cdr bit-list) (cdr t)))
   (else (cons (car t)
               (encode-bit-list (cdr bit-list) (cdr t))))))

(define (every-combination t)
  (map (cut encode-bit-list <> t)
       (bit-range (expt 2 (- (length t) 1)))))

(define (every-interpolation interval pscale)
  (let ((rotations (all-rotations interval pscale)))
    (map combine-rotations
         (every-combination rotations))))

(define (molts n)
  (remove-duplicates/lists
   (apply append
          (map (lambda (prime)
                 (every-interpolation prime (range n)))
               (prime-symmetrical-scales n)))))

(define (symmetrical-scales n-tones)
  (let ((halfway (quotient (+ n-tones 1) 2)))
    (map (lambda (scale)
           (append scale (cdr (map (cut + halfway <>) scale))))
         (every-combination (range (+ halfway 1))))))


;- sorted molts algorithm -------------------------


;- finding "pure" or "true" molts -----------------
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


;- printing molts ---------------------------------
(define (print-molts n)
  (let ((molts (molts n)))
    (print "** " n " tone scale\n")
    (print "** prime symmetrical scales")
    (for-each print (prime-symmetrical-scales n))
    (print "\n** molts (" (length molts) ")")
    (for-each print molts)
    (print "\n** pure molts")
    (for-each print (pure-molts molts))))

(define (print-molt-to-file n)
  (with-output-to-file
      (cond
       ((< n 10) (format "scales/molts00~a.txt" n))
       ((< n 100) (format "scales/molts0~a.txt" n))
       (else (format "scales/molts~a.txt" n)))
    (cut print-molts n))
  (print "finished " n))

(define (print-all-molts-to-file)
  (for-each print-molt-to-file
            (filter (compose not prime?)
                    (map (cut + 2 <>)
                         (range 23)))))

(define (print-translated-molts n pscale)
  (let* ((translate-to-c
          (lambda (scales)
            (map (cut translate-row <> (range n) pscale) scales)))
         (molts (molts n))
         (primes (translate-to-c (prime-symmetrical-scales n)))
         (pures (translate-to-c (pure-molts molts)))
         (molts (translate-to-c molts)))
    (print "** " n " tone scale\n")
    (print "** prime symmetrical scales")
    (for-each print primes)
    (print "\n** molts (" (length molts) ")")
    (for-each print molts)
    (print "\n** pure molts")
    (for-each print pures)))

(define (print-translated-molts-to-file n pscale)
  (with-output-to-file
      (cond
       ((< n 10) (format "scales/molts00~a-translated.txt" n))
       ((< n 100) (format "scales/molts0~a-translated.txt" n))
       (else (format "scales/molts~a-translated.txt" n)))
    (cut print-translated-molts n pscale))
  (print "finished " n " (translated)"))

(define (main)
  (print-translated-molts-to-file 4 (take-every 12-tone-row:c 3))
  (print-translated-molts-to-file 6 (take-every 12-tone-row:c 2))
  (print-translated-molts-to-file 12 12-tone-row:c)
  (print-translated-molts-to-file 24 24-tone-row:c)
  (print-translated-molts-to-file 8 (take-every 24-tone-row:c 3))
  (print-all-molts-to-file)
  (print-molt-to-file 25)
  (print-molt-to-file 49)
  (print-molt-to-file 121)
  (print-molt-to-file 169))

(take-every 24-tone-row:c 2)
