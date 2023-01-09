
(define-datatype prefix-exp prefix-exp?
  (const-exp
   (num integer?))
  (diff-exp
   (operand1 prefix-exp?)
   (operand2 prefix-exp?)))

(define parse-prefix
  (lambda [exp]
  (parse-prefix* (reverse exp) '())))

(define parse-prefix*
  (lambda [exp lst]
    (cond
      [(null? exp) (car lst)]
      [(integer? (car exp)) (parse-prefix* (cdr exp) (cons (const-exp (car exp)) lst))]
      [(equal? '- (car exp)) (parse-prefix* (cdr exp) (cons (diff-exp (car lst) (cadr lst)) (cddr lst)))])))


(define unparse-prefix
  (lambda [exp]
    (cases prefix-exp exp
      [const-exp [num] (list num)]
      [diff-exp [operand1 operand2]
        (append '(-) (unparse-prefix operand1) (unparse-prefix operand2))])))

(define eval-prefix
	(lambda [exp]
		(cases prefix-exp exp
			[const-exp [num] num]
			[diff-exp [operand1 operand2]
				(- (eval-prefix operand1) (eval-prefix operand2))])))
