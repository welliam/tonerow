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

(define (id x) x)
