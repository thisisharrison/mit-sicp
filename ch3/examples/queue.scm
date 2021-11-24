; Queue (FIFO)
; Inserted from rear and deleted from front

; a constructor:
; (make-queue)

; two selectors:
; (empty-queue? <queue>)
; (front-queue <queue>)

; two mutators:
; (insert-queue! <queue> <item>)
; (delete-queue! <queue>)

; representation: 
; pair of pointers: front-ptr and rear-ptr. front-ptr's cdr is index 1 in the queue

; ===== QUEUE =====
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))
(define (empty-queue? queue) (null? (front-ptr queue)))

; front and rear pointer
(define (make-queue) (cons '() '()))
(define (front-queue queue)
    (if (empty-queue? queue)
        (error "FRONT called with empty queue" queue)
        ; (cons '() '()) ; -> get the car of first pair in queue
        (car (front-ptr queue))))
(define (insert-queue! queue item)
; create a new pair whose car is the item to be inserted and cdr is a empty list
    (let ((new-pair (cons item '())))
        ; using the new pair we created, if empty queue set front and rear to node 
        (cond ((empty-queue? queue)
            (set-front-ptr! queue new-pair)
            (set-rear-ptr! queue new-pair)
            ; return queue
            queue)
            ; not empty
            (else 
            ; mutate rear ptr to point to new pair -> set-cdr! -> set-y! (see above)
            ; in example below, set pointer (3 '()) to (3 (4 '())) -> ((1 2 3 4)...
            (set-cdr! (rear-ptr queue) new-pair)
            ; assign rear pointer to new pair -> set-cdr! -> set-y!
            ; in example below, ((...) 4)
            (set-rear-ptr! queue new-pair)
            ; return queue
            queue))))

(define (delete-queue! queue)
    (cond ((empty-queue? queue)
        (error "DELETE! called with empty queue" queue))
        (else
        ; set-car! queue cdr of front-ptr
        ; (1 2 3) -> cdr -> (2 3)
        (set-front-ptr! queue (cdr (front-ptr queue)))
        ; return in else condition
        queue)))

(define q (make-queue))
(insert-queue! q 1) ; ((1) 1)
(insert-queue! q 2) ; ((1 2) 2)
(insert-queue! q 3) ; ((1 2 3) 3)

(front-ptr q) ; (1 2 3)
(rear-ptr q) ; (3)

(delete-queue! q) ; ((2 3) 3)

