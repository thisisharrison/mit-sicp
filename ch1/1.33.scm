(define (filtered-accumulate combiner null-value a filter term b next)
    (if (> a b) 
        null-value
        (combiner 
                ; apply filter here, if filter a satisfied, term a, otherwise user null value
                (if (filter a) (term a) null-value)
                (filtered-accumulate combiner null-value (next a) filter term b next))))

(define (inc a) (+ a 1))
(define (identity a) a)
(define (even? a)
    (= (modulo a 2) 0))

(even? 2)
; #t
(even? 3)
; #f
(even? 4)
; #t

(filtered-accumulate + 0 1 even? identity 10 inc)
; 2 + 4 + 6 + 8 + 10 = 30
;Value: 30

(filtered-accumulate * 1 1 even? identity 5 inc)
; 2 * 4 = 8
;Value: 8

(define (filtered-accumulate-iter combiner null-value a filter term b next)
    (define (iter a res)
        (if (> a b)
            res
            (iter (next a)
                    (combiner (if (filter a) (term a) null-value) res))))
    (iter a null-value))

(filtered-accumulate-iter + 0 1 even? identity 10 inc)
;Value: 30

(filtered-accumulate-iter * 1 1 even? identity 5 inc)
;Value: 8