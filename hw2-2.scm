;;1 GRADE
(define (times10* nums)
   (* 10 nums))

(define times10
    (lambda [num-ls]
        (map times10* num-ls)))

;; 2  GRADE
(define pair-up
    (lambda [elt ls]
        (map (lambda [x] (cons elt x)) ls)))

;;4 GRADE
(define replace
  (lambda [old new syms]
    (cond
      [(list? syms) (map (lambda [lst] (replace old new lst)) syms)]
      [else (if (equal? syms old) new syms)])))

;;5 GRADE
(define remove
  (lambda [elt ls]
    (filter 
      (lambda [x] (not (equal? x elt))) ls)))

;;7 GRADE
(define length
  (lambda [ls]
    (fold-left
      (lambda [x y] (+ x 1)) 0 ls)))

;;8 GRADE
(define average
  (lambda [nums]
    (/ (fold-right (lambda (x y) (+ x y)) 0 nums) 
     (length nums))))

;;10 GRADE
(define reverse
  (lambda  [ls]
     (fold-right (lambda (x y) (append y (list x))) '() ls)))
