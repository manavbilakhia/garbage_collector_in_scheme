;; store.scm
(define the-store! 'uninitialized)

;; (empty-store) returns an empty Scheme list representing the empty store.
(define empty-store (lambda () '()))

;; (initialize-store!) initializes the-store! to (empty-store).
(define initialize-store!
  (lambda () (set! the-store! (empty-store)) ))

;; (newref! ev) takes an expval ev adds to the-store! and returns
;; a ref-val that points to the added value.
(define newref!
  (lambda (ev)
    (set! the-store! (append the-store! (list ev)))
    (ref-val (- (length the-store!) 1))
    ))

;; (deref ev) takes an expressed value which should be a (ref-val ref)
;; and returns the value associated with ref in the-store!.
(define deref
  (lambda (ev)
    (list-ref the-store! (expval->ref ev))
    ))



;; (setref! ev1 ev2) takes two expvals, the first which should be a
;; (ref-val ref) it sets the cell ref in the-store! to ev2.  Returns (unit-val).
(define setref!
  (lambda (ev1 ev2)
    (let [[ref (expval->ref ev1)]]
      (set! the-store! (setref!* the-store! ref ev))
      (unit-val)
    )))


(define setref!*
  (lambda (store ref ev)
    (cond
      [(= 0 ref) (cons ev (cdr store))]
      [else (cons (car store) (setref!* (cdr store) (- ref 1) ev))])))
  
