(define lc1 'x )
(define lc2 'y )
(define lc3 '(lambda (x) x) )
(define lc4 '(lambda (y) x) )
(define lc5 '(x y) )
(define lc6 '(x (y y)) )
(define lc7 '(x (lambda (y) y)) )
(define lc8 '(lambda (x) (x y)) )
(define lc9 '(lambda (x) (lambda (y) x)) )
(define lc10 '((lambda (x) x) (lambda (y) y)) )

;;1
(define lambda?
  (lambda (exp)
    (and (list? exp)                 
		 (equal? (length exp) 3)
		 (equal? (car exp) 'lambda)
		 (list? (cadr exp))
		 (equal? (length (cadr exp)) 1)
		 (symbol? (caadr exp)))))

(define var?
  (lambda (exp)
    (symbol? exp)))

(define apply?
  (lambda (exp)
    (and (list? exp)
		 (equal? (length exp) 2))))

(define get-lvars
  (lambda [exp]
    (get-lvars* exp '())))

(define get-lvars*
    (lambda [exp var-list]
    (cond

        [(null? exp) var-list]
        [(var? exp) (cons exp var-list)]
        [(lambda? exp) (get-lvars* (caddr exp) var-list)]
			[(apply? exp)
				(append (get-lvars* (car exp) var-list) (get-lvars* (cadr exp) var-list))])))



;;2
(define  get-lparams
    (lambda [exp]
        (reverse (get-lparams* exp '()))))

(define get-lparams*
    (lambda [exp param-list]
    (cond
        [(null? exp) param-list]
        [(var? exp) param-list]
        [(lambda? exp) (get-lparams* (caddr exp) (append (cadr exp) param-list))]
			[(apply? exp)
				(append param-list (append (get-lparams* (car exp) '()) (get-lparams* (cadr exp) '())))])))

;;3
(define replace-vars
    (lambda [exp]
        (replace-vars* exp 0)))

(define replace-vars*
    (lambda [exp depth]
        (cond
            [(var? exp) depth]
            [(lambda? exp) (list (car exp) (cadr exp) (replace-vars* (caddr exp) (+ 1 depth)))]
            [(apply? exp) (list (replace-vars* (car exp) depth) (replace-vars* (cadr exp) depth))])
            ))
;;4
(define free-vars
  (lambda (exp)
    (free exp '())))

(define free
  (lambda (exp formals-seen)
    (cond
       [(var? exp) (if (fold-left (lambda [a b] (or (equal? exp b) a)) #f formals-seen)'() (list exp))]
       [(lambda? exp) (free (caddr exp) (append (cadr exp) formals-seen))]
       ;;[(apply? exp) (append formals-seen (append (free (car exp) '()) (free (cadr exp) '())))]
       [(apply? exp) (append (free (car exp) formals-seen) (free (cadr exp) formals-seen))])))
