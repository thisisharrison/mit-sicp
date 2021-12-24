; recursive
(define (f n)
    (cond ((< n 3) n)
        (else (+ (f (- n 1))
                (* 2 (f (- n 2)))
                (* 3 (f (- n 3)))
                ))))

(f 10)
;Value: 1892

; f(n -1) + 2f(n - 2) + 3f(n - 3)
; a -> a + 2b + 3c
; b -> a
; c -> b

; iterative
(define (f n)
    (cond ((< n 3) n)
    (else (f-iter 2 1 0 (- n 2)))))
        
(define (f-iter a b c count)
    (if (= count 0)
        a
        (f-iter (+ a (* 2 b) (* 3 c)) a b (- count 1))))

(f 10)
;Value: 1892