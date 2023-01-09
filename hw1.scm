; Collaborated with James Heffernan, Daniella Massa, Ami Ivoko 
; 1
(+ 41 1)
;; 2
3.1415   
;; 3
"hello world"
;; 4
"Let the Grecian dream of his sacred stream"   
;; 5
(+ 3 4)
;; 6
(* (+ (* 4 (/ 5 9)) 17) (- 6.7 13)) 
;; 7
(+ 0 1 2 3 4 5 6 7)  
;; 8
(define x "hello world")
;; 9
(list? 6)
;; 10
 (lambda [x] x) 
;; 11
(define (identity e) e)
;; 12
(identity x)
;; 13
(let [[a 2] [b 7] [c 18]] (/ (+ (- b) (expt (- (expt b 2) (* 4 a c)) (/ 1 2))) (* 2 a)))
;; 14
(define plus42
    (lambda [e]
        (+ 42 e)))
;; 15
(car (list 1 1 2 3 5))
;; 16
(cadddr (list 1 1 2 3 5 ))
;; 17
(cons 1 '(1 2 3 5)) 
;; 18
(cons 3 4)
;; 19
(cons (cons (cons 1 2) '(3 4 )) 5)
;; 20
(and (or #t #f) #t)
;; 21
(define (xor d e)
    (if d (not e) e))
(let [[a #t] [b #f] [c #f]] (or (or (xor a (not b)) (and c (not a))) b))
;; 22
(if (equal? x 42) "yes" "no")
;; 23
(define positive?
    (lambda [e]
        (cond
            [(> e 0) #t]
            [else #f])))
;; 24
(define numMonth->strMonth
    (lambda [n]
        (cond
            [(= n 1) "January"]
            [(= n 2) "February"]
            [(= n 3) "March"]
            [(= n 4) "April"]
            [(= n 5) "May"]
            [(= n 6) "June"]
            [(= n 7) "July"]
            [(= n 8) "August"]
            [(= n 9) "September"]
            [(= n 10) "October"]
            [(= n 11) "November"]
            [(= n 12) "December"]
            [else "invalid input"])))
;; 25
(define list-member?
    (lambda [e ls]
        (cond
        [(null? ls) #f]
        [(equal? (car ls) e) #t]
        [else (list-member? e (cdr ls))])))
;; 26
(define range
    (lambda [num1 num2]
        (cond 
            [(< num2 num1) (list )]
            [(= num1 num2) (list num1)]
            [else (cons num1 (range (if (> num2 num1) (+ 1 num1) num2) num2))])))
;; 27
(define list-append
    (lambda [ls1 ls2]
        (cond 
            [(null? ls1) ls2]
            [else (cons (car ls1) (list-append (cdr ls1) ls2))])))
;; 28
(define list-flatten 
    (lambda [lls]
        (cond
            [(null? lls) (list )]
            [else (append  (car lls) (list-flatten (cdr lls)))])))
;; 29
(define list-map
    (lambda [fn ls]
        (cond
            [(null? ls) (list )]
            [else (cons (fn (car ls)) (list-map fn (cdr ls)))])))
;; 30
(define list-filter
    (lambda [p? ls]
        (cond
            [(null? ls) (list )]
            [(p? (car ls)) (cons (car ls) (list-filter p? (cdr ls)))]
            [else (list-filter p? (cdr ls))])))
;; 31
(define list-counts 
    (lambda [p? ls]
        (cond
            [(null? ls) 0]
            [(p? (car ls)) (+ 1 (list-counts p? (cdr ls)))]
            [else (list-counts p? (cdr ls))])))
;; 32**************** incomplete****************

;(define fib2            
;    (lambda [n]
;        (cond
;        [(= 0 n) (list )]
;        [(= 1 n) (list 0)]
;        [(= 2 n) (list 0 1)]
;        [else (append )]
;        [else (+ (fib2 (- n 1)) (fib2 (- n 2)))])))
;; 33
(define list-insert
    (lambda [num ls]
    (cond
    [(null? ls) (list num)]
    [(<= num (car ls)) (cons num ls)]
    [else (cons (car ls) (list-insert num (cdr ls)))])))
;; 34
(define list-insertion-sort
    (lambda [nums]
        (cond
        [(null? nums) (list )]
        [else (list-insert (car nums) (list-insertion-sort (cdr nums)))])))
;;35 ******************incomplete*************************
(define score100f
    (lambda [f]
        (cond
        [(>= 100 f) ])))
;;36
(define list-merge
    (lambda [nums1 nums2]
    (cond
        [(null? nums1) nums2]
        [(null? nums2) nums1]
        [(>= (car nums1) (car nums2)) (cons (car nums2) (list-merge nums1 (cdr nums2)))]
        [else (cons (car nums1) (list-merge (cdr nums1) nums2))])))
;;37*********incomplete********
;; write a function to generate a list with all the odd indexes of a given list called split-odd that takes 1 list as a param
;; write a function to generate a list with all the even indexes of a given list called split-even that takes 1 list as a param
(define list-mergesort 
    (lambda [nums]
    (cond
        [(null? nums) nums]
        [(null? (cdr nums)) nums]
        [else (list-merge (list-mergesort (split-odd nums)) (list-mergesort (split-even nums)))])))















