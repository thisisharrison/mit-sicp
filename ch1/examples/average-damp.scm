; f is a function
(define (average-damp f)
    (lambda (x) (average x (f x))))

(define (square x) (* x x))

(define (average a b) (/ (+ a b) 2))

; Average Damp takes the function square and returns a function 
; takes arg x and return average of x and square x 

((average-damp square) 10)
;Value: 55

(average 100 10)
;Value: 55

