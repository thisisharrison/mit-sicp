; Tables
; *table* -> (() ()->(() ()-> (() ())) 
;             a: 1
;                     b: 2
;                              c: 3 


; assoc: search the key
(define (assoc key records)
    (cond ((null? records) false)
        ; (car (car record)) -> gets a, b, c
        ; (car records) -> get (a 1)
        ((equal? key (caar records)) (car records))
        ; continue with ((b 2) (c 3))
        (else (assoc key (cdr records)))))


; key is for eg. a, b, c
(define (lookup key table)
    (let ((record (assoc key (cdr table))))
        (if record
            ; gets 1 for a
            (cdr record)
            false)))

; use assoc to see if there's already a record with this key
(define (insert! key value table)
    (let ((record (assoc key (cdr table))))
        (if record
            ; if yes, update cdr of the record 
            (set-cdr! record value)
            (set-cdr! table
                    ; if no, insert at the head 
                    (cons (cons key value) (cdr table)))))
    'ok)

; make table
(define (make-table) (list '*table*))

(define t (make-table)) ; t
(insert! 'a 1 t) ; ok
(insert! 'b 1 t) ; ok
(lookup 'a t) ; 1
(lookup 'b t) ; 1
(insert! 'b 2 t) ; ok
(lookup 'b t) ; 2

