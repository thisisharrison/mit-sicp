#lang racket
(require (submod "../math.rkt"))

(slow-prime? 3) ;Value: #t
(slow-prime? 4) ;Value: #f 
(slow-prime? 5) ;Value: #t
(slow-prime? 6) ;Value: #f
(slow-prime? 7) ;Value: #t
(slow-prime? 11) ;Value: #t
(slow-prime? 13) ;Value: #t
(slow-prime? 17) ;Value: #t
