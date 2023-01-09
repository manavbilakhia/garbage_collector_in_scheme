;; interface defination
(define zero
	(lambda [] '(0)))


(define is-Zero?
	(lambda (ls)
        (if (null? ls) #t
		(and (equal? (car ls) 0) (is-Zero? (cdr ls))))
        ))

(define successor
    (lambda [ls]
    (successor* (reverse ls) '())))

(define successor*
    (lambda [ls res]
        (cond
            [(null? ls) (reverse (append res '(1)))]
            [(= 0 (car ls)) (reverse (cons 1 (append (cdr ls) res)))]
            [(= 1 (car ls)) (append (successor* (cdr ls) '()) '(0))]
            )))
(define predecessor
    (lambda [ls]
        (predecessor* (reverse ls) '())))

(define predecessor*
    (lambda [ls res]
        (cond
            [(is-Zero? ls) '(0)]
            [(= 1 (car ls)) (reverse (cons 0 (append (cdr ls) res)))]
            [(= 0 (car ls)) (append (predecessor* (cdr ls) '()) '(1))])))

(define binary->number
    (lambda [ls]
         (binary->number* ls 0)))

(define binary->number*
    (lambda [ls num]
        (cond
            [(is-Zero? ls) num]
            [else (binary->number* (predecessor ls) (+ 1 num))])))

(define number->binary
    (lambda [num]
         (number->binary* '(0) num)))

(define number->binary*
    (lambda [ls num]
        (cond
            [(= 0 num) ls]
            [else (number->binary* (successor ls) (- num 1))])))

(define sum
  (lambda [a b]
    (if (is-Zero? a) 
			b
			(sum (predecessor a) (successor b)))))

(define prod
  (lambda [a b]
    (if (is-Zero? a) 
			(zero)
			(sum b (prod (predecessor a) b)))))

(define less-than?
    (lambda [a b]
        (less-than?* (successor a) b)))


(define less-than?*
  (lambda (a b)
    (if (is-Zero? a)
			#t
			(if (is-Zero? b)
				#f
				(less-than?* (predecessor a) (predecessor b))))))

(define equals?
    (lambda [a b]
        (cond
            [(and (is-Zero? a) (is-Zero? b)) #t]
            [(or (is-Zero? a) (is-Zero? b)) #f]
            [else (equals? (predecessor a) (predecessor b))])))



(define remove-zeros
    (lambda [a]
        (cond 
            [(= 0 (car a)) (remove-zeros (cdr a))]
            [else a])))
(define equals-binary?
    (lambda [a b]
    (equals-binary?* (remove-zeros a) (remove-zeros b))))

(define equals-binary?* 
    (lambda [a b]
        (cond 
        [(and (null? a) (null? b)) #t]
        [(null? a) #f]
        [(null? b) #f]
        [(= (car a) (car b)) (equals-binary?* (cdr a) (cdr b))]
        [else #f])))
(define less-than-binary?
    (lambda [a b]
    (less-than-binary?* (remove-zeros a) (remove-zeros b))))

(define less-than-binary?*
    (lambda [a b]
    (cond
        [(> (length a) (length b)) #f]
        [(= (car a) (car b)) (less-than-binary?* (cdr a) (cdr b))]
        [(> (car a) (car b)) #f]
        [else #t])))

(define sum-binary
    (lambda [a b]
        (reverse (sum-binary* (reverse (remove-zeros a)) (reverse (remove-zeros b)) 0))))

(define sum-binary*
    (lambda [a b carry]
        (cond
            [(and (null? a) (null? b)) 
                (if (= carry 0) '( ) '(1))]
            [(null? b) (sum-binary* a '(0) carry)]
            [(null? a) (sum-binary* b '(0) carry)]
            [else 
                (let ([num-a (car a)]
                      [num-b (car b)])
                        (cond
                             [(=(+ num-a num-b carry) 0) (cons 0 (sum-binary* (cdr a)(cdr b) 0))]
                             [(=(+ num-a num-b carry) 1) (cons 1 (sum-binary* (cdr a)(cdr b) 0))]
                             [(=(+ num-a num-b carry) 2) (cons 0 (sum-binary* (cdr a)(cdr b) 1))]
                             [else (cons 1 (sum-binary* (cdr a) (cdr b) 1))]))])))

