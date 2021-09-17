; METHOD 1
(define (sum-of-squares-of-two-largest a b c)
  (cond
    ((and(> a b)(> c b)) (sum-of-squares a c))
    ((and(> b a)(> c a)) (sum-of-squares b c))
    ((and(> a c)(> b c)) (sum-of-squares a b))
  )
)

; METHOD 2: sum of all square minus square of min
(define (sum-square-two-larger a b c) 
  (- (+ (square a) 
        (square b) 
        (square c))
     (square (min a b c))))

(define (sum-of-squares a b)
    (+ (square a )(square b)))

(define (square num) (* num num))

; (sum-of-squares-of-two-largest 5 6 1)
(sum-square-two-larger 5 6 1) 