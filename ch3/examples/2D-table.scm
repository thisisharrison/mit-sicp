; Tables
; *table* -> (() ()->(() ())
;             math    letters
;              |      |_ (a 47) -> (b 98)
;              | 
;              |_ (+ 43) -> (- 45)


; assoc: search the key
(define (assoc key records)
    (cond ((null? records) false)
        ; (car (car record)) -> gets a, b, c
        ; (car records) -> get (a 1)
        ((equal? key (caar records)) (car records))
        ; continue with ((b 2) (c 3))
        (else (assoc key (cdr records)))))


; k1 like letters, k2 like `a`
(define (lookup k1 k2 table)
    (let ((subtable (assoc k1 (cdr table))))
        (if subtable
            ; get the value in subtable
            (let ((record (assoc k2 (cdr subtable))))
                (if record
                    (cdr record)
                    false))
            ; no subtable, return false
            false)))

(define (insert! k1 k2 value table)
    (let ((subtable (assoc k1 (cdr table))))
        (if subtable
            (let ((record (assoc k2 (cdr subtable))))
                (if record
                    (set-cdr! record value)
                    ; update subtable
                    (set-cdr! subtable
                            (cons (cons k2 value) (cdr subtable)))))
            ; no subtable: create new k1 k2 value 
            (set-cdr! table
                    ; letter - a - 1
                    (cons (list k1 (cons k2 value)) (cdr table)))))
    'ok)

; make table
(define (make-table) (list '*table*))

(define t (make-table)) ; t
(insert! 'letter 'a 1 t) ; ok
(insert! 'letter 'b 1 t) ; ok
(insert! 'math '+ 2 t) ; ok
(insert! 'math '- 2 t) ; ok
(lookup 'letter 'b t) ; 1
(lookup 'math '+ t) ; 2

(insert! 'math '+ 100 t) ; ok
(lookup 'math '+ t) ; 100