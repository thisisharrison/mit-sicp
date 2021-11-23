; Mutable list 
(define (cons x y)
    (define (set-x! v) (set! x v))
    (define (set-y! v) (set! y v))
    (define (dispatch m)
        (cond ((eq? m 'car) x)
            ((eq? m 'cdr) y)
            ((eq? m 'set-car!) set-x!)
            ((eq? m 'set-cdr!) set-y!)
            (else (error "Undefined operation --CONS" m))))
    dispatch)

; z is a pair
(define (car z) (z 'car))
(define (cdr z) (z 'cdr))
(define (set-car! z new-val)
    ; (z 'set-car!) returns set-x! function and takes new-val as v 
    ((z 'set-car!) new-val)
    z) ; return z
(define (set-cdr! z new-val)
    ((z 'set-cdr!) new-val)
    z)

(define x (cons 1 2))
(car x) ; 1
(cdr x) ; 2
(set-car! x 10)
(car x) ; 10
(set-cdr! x 20)
(cdr x) ; 20

