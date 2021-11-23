; Sharing identity

(define x (list 'a 'b))
(define z (cons x x))

(display x) ; (a b)
(display z) ; ((a b) a b)

(eq? (car z) x) ; #t, they are the same object, same pointers 
