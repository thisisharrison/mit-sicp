 (define (pascal r c) 
    ; if col is 1 (left bound) or row = col (right bound), return 1
    (if (or (= c 1) (= r c))
        1
        ; add two numbers of last row
        ; one on the left col, one on the same col
            ; 3 1
            ;  4
            ; 1 and 4 are the same col
        (+ (pascal (- r 1)(- c 1)) (pascal (- r 1) c) )))
  

 (pascal 1 1) 
 (pascal 2 2) 
 (pascal 3 2) 
 (pascal 4 2) 
 (pascal 5 2) 
 (pascal 5 3) 

; 1;     1
; 2;    1 1  
; 3;   1 2 1 
; 4;  1 3 3 1 
; 5; 1 4 6 4 1