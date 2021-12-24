#lang sicp

;; Computes Rational Numbers and Ordidinary Numbers
;; Use concept of "packages"
; (put ⟨op ⟩ ⟨type ⟩ ⟨item ⟩) installs the ⟨item ⟩ in the table, indexed by the ⟨op ⟩ and the ⟨type ⟩.
; (get ⟨op ⟩ ⟨type ⟩) looks up the ⟨op ⟩, ⟨type ⟩ entry in the table and returns the item found there. If no item is found, get returns false.

(define (apply-generic op arg) (arg op))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (attach-tag symbol x) (cons symbol x))

(define (install-ordinary-number-package)
    (define (tag x) 
        (attach-tag 'ordinary x))
    (put 'add '(ordinary ordinary)
        (lambda (x y) (tag (+ x y))))
    (put 'sub '(ordinary ordinary)
        (lambda (x y) (tag (- x y))))
    (put 'mul '(ordinary ordinary)
        (lambda (x y) (tag (* x y))))
    (put 'div '(ordinary ordinary)
        (lambda (x y) (tag (/ x y))))
    (put 'make 'ordinary
        (lambda (x) (tag x)))
    'done)

; Create tagged numbers
(define (make-ordinary n) ((get 'make 'ordinary) n))

(define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))))

(define (install-rational-package)
  ;; internal procedures
    (define (numer x) (car x))
    (define (denom x) (cdr x))
    (define (make-rat n d)
        (let ((g (gcd n d)))
            (cons (/ n g) (/ d g))))
    (define (add-rat x y)
        (make-rat (+ (* (numer x) (denom y))
            (* (numer y) (denom x)))
            (* (denom x) (denom y))))
    (define (sub-rat x y)
        (make-rat (- (* (numer x) (denom y))
            (* (numer y) (denom x)))
            (* (denom x) (denom y))))
    (define (mul-rat x y)
        (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
    (define (div-rat x y)
        (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
    (put 'add '(rational rational)
        (lambda (x y) (tag (add-rat x y))))
    (put 'sub '(rational rational)
        (lambda (x y) (tag (sub-rat x y))))
    (put 'mul '(rational rational)
        (lambda (x y) (tag (mul-rat x y))))
    (put 'div '(rational rational)
        (lambda (x y) (tag (div-rat x y))))
    (put 'make 'rational
        (lambda (n d) (tag (make-rat n d))))
    'done)

(define (make-rational n d)
    ((get 'make 'rational) n d))

;; put and get implementation

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))


(install-ordinary-number-package)
(install-rational-package)

;     |   ordinary | rational
; ------------------------------
; add |            | 
; sub |            |
; mul |            |
; div |            |

(define ten (make-ordinary 10))
(define five (make-ordinary 5))

(display ten) 
; (ordinary . 10)
; (add ten five)

(define one-half (make-rational 1 2))
(display one-half)
; (rational 1 . 2)

; TODO: apply the procedures