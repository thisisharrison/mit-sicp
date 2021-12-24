#lang racket 

(provide slow-prime? prime? gcd deriv)
(require (submod "math/prime.rkt") 
        (submod "math/gcd.rkt") 
        (submod "math/deriv.rkt"))