(define (accumulate combiner null-value a term b next)
    (if (> a b) 
        null-value
        (combiner (term a)
                (accumulate combiner null-value (next a) term b next))))

(define (inc a) (+ a 1))
(define (identity a) a)

(accumulate + 0 1 identity 10 inc)
;Value: 55

(accumulate * 1 1 identity 5 inc)
;Value: 120

(define (accumulate-iter combiner null-value a term b next)
    (define (iter a res)
        (if (> a b)
            res
            (iter (next a) (combiner (term a) res))))
    (iter a null-value))

(accumulate-iter + 0 1 identity 10 inc)
;Value: 55
(accumulate-iter * 1 1 identity 5 inc)
;Value: 120