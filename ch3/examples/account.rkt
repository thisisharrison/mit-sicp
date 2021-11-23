#lang sicp

;; Computing objects with assignment of Local Variables and set state
;; Set up an environment with local state balance
;; Closure: Access to local variables binding like balance

; begin: if predicate is true, all following expressions are evaluated and the last one is returned
(define (withdraw)
    (let ((balance 100))
        (lambda (x)
            (if (>= balance x)
                (begin (set! balance (- balance x)) balance)
                "Insufficient Funds"))))

(display "@@WITHDRAW\n")
(define A0 (withdraw))
(A0 10) ;90
(A0 20) ;70


(define (make-withdraw balance)
    (lambda (x)
        (if (>= balance x)
            (begin (set! balance (- balance x)) balance)
            "Insufficient Funds")))

(define account1 (make-withdraw 100))
(define account2 (make-withdraw 100))

(display "@@MAKE-WITHDRAW\n")
(account1 50)   ;50
(account1 20)   ;30
(account1 30)   ;0
(account1 10)   ;"Insufficient Funds"

(account2 10)   ;90
(account2 100)  ;"Insufficient Funds"


; Use Dispatch (message-passing style of programming)
; Returns bank-account object 
(define (make-account initial-amount)
    (let ((balance initial-amount))
        (define (withdraw x)
            (if (>= balance x)
                (begin (set! balance (- balance x)) balance)
                "Insufficient Funds"))
        (define (deposit x)
            (set! balance (+ balance x))
            balance)
        (define (dispatch action)
            ; if action symbole equals withdraw, return the withdraw procedure
            (cond 
                ((eq? action 'withdraw) withdraw)
                ((eq? action 'deposit) deposit)
                (else (error "Unknown action: MAKE-ACCOUNT" action))))
        dispatch))

(display "@@MAKE-ACCOUNT\n")
(define A1 (make-account 100))
; (A1 'withdraw) => dispatch 'withdraw => (withdraw 10)
((A1 'withdraw) 10)     ;90
((A1 'deposit) 20)      ;110
((A1 'withdraw) 110)    ;0

