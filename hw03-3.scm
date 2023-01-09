(define true
  (lambda (a)
    (lambda (b)
      a)))

(define false
  (lambda (a)
    (lambda (b)
      b)))

(define if-ba 
  (lambda (test exp1 exp2)
    ((test exp1) exp2)))

(define not-ba
  (lambda [a]
    (if-ba a false true)))

(define and-ba
  (lambda [a b]
  (if-ba b (if-ba a true false) false)))

(define xor-ba
  (lambda [a b]
  (if-ba b (if-ba a false true) false)))

(define ba->boolean
  (lambda [ba]
    (if-ba ba #t #f)))
  
(define boolean->ba
  (lambda [bool]
    (cond
      [bool true]
      [else false])))
