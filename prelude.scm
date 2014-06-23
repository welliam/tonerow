(define (range n)
  ; useful for creating the twelve tone row, [0..11]
  (if (negative? n)
      (error "No negative arguments to range: " n)
      (let loop ((i 0))
        (if (>= i n)
            '()
            (cons i (loop (+ i 1)))))))

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
  (syntax-rules ()
    ((_ ((arg . ps) ...) body ...)
     (lambda (arg ...)
       (if (and ((apply compose-predicates (list . ps)) arg) ...)
           (begin body ...)
           (error
            "Bad argument given to function. Arguments and predicates:\n"
            `((,arg . ps) ...)))))))
