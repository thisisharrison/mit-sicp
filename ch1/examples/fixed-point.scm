(define tolerance 0.00001)
(define (fixed-point f first-guess)
    (define (close-enough? x y)
        (< (abs (- x y)) tolerance))
    (define (try guess)
        (let (
                (next (f guess)) ; f(guess) = next
            ) 
            ; body of let
            (if (close-enough? guess next)
            next
            (try next))))
    (try first-guess))


(fixed-point cos 1.0)
;Value: .7390822985224023


; (let (
;         (⟨var1⟩ ⟨exp1⟩) 
;         (⟨var2⟩ ⟨exp2⟩)
;         ...
;         (⟨varn⟩ ⟨expn⟩)
;     ) 
;     ⟨body⟩)

; y = sqrt x
; y = (x / y + y) / 2

(define (sqrt x)
    (fixed-point (lambda (y) (average (/ x y) y))
                1.0))
(define (average x y)
    (/ (+ x y) 2))

(sqrt 9)
;Value: 3
(sqrt 25)
;Value: 5