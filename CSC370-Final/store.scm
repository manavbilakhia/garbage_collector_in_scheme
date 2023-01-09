;; store.scm

;; ==================== Free cells ====================================

;; A stack of store refs that are free
;; All refs in this list are less than store-length!
(define free-cells! '())

(define free-cell-exists? (lambda ()
  (not (null? free-cells!))
))

;; Adds a free cell to the free-cells! stack
(define add-free-cell! (lambda (cell)
  (set! free-cells! (cons cell free-cells!))
))

;; Pops a value from the top of the free-cells! stack
(define pop-free-cell! (lambda () (let 
  ([ref (car free-cells!)])
  (set! free-cells! (cdr free-cells!)) ref)))

;; increment the reference count of an element in the store
(define increment_count! (lambda (ev)
  (let [[ref (expval->ref ev)]]
    (let ([cell (vector-ref the-store! ref)])
      (cases store-cell cell
            [ref-cell (val count) (vector-set! the-store! ref (ref-cell val (+ count 1))) (unit-val)]
            [empty-cell () (raise-exception 'increment_count! "attempted to increment the count of an empty cell at index: ~s" ref)]
            )))))

;; decrement the reference count of an element in the store
(define decrement_count! (lambda (ev)
  (let [[ref (expval->ref ev)]]
    (let ([cell (vector-ref the-store! ref)])
      (cases store-cell cell
            [ref-cell (val count) (vector-set! the-store! ref (ref-cell val (- count 1))) (if (= (- count 1) 0) (remove-cell! ev) (unit-val))]
            [empty-cell () (raise-exception 'decrement_count! "attempted to decrement the count of an empty cell at index: ~s" ref)]
            )))))

;;removes a cell from the store
  (define remove-cell! (lambda (ev)
    (let [[ref (expval->ref ev)]]
      (let ([cell (vector-ref the-store! ref)])
        (cases store-cell cell
              [ref-cell (val count) 
                (cases expval val
                  [proc-val (params body saved-env) (decrement-all saved-env)]
                  [ref-val (ref) (decrement_count! val)]
                  [else 'donothing ]
                )
                (vector-set! the-store! ref (empty-cell)) 
                (add-free-cell! ev) 
                (unit-val)
              ]
              [empty-cell () (raise-exception 'increment_count! "attempted to remove an empty cell at index: ~s" ref)]
              )))))

;; ==================== Store ====================================

(define the-store! 'uninitialized)

;; The current number of cells in the store that either contain a value or have contained a value
;; There may be some cells before this that are empty vals due to garbage collection
;; Note, the current capacity of the store is different from it's length
(define store-length! 0)

;; Represents a cell in the store
(define-datatype store-cell store-cell?
  ;; Represents a cell that is currently holding a value
  ;; Holds a count of how many objects in a program are currently referencing it
  [ref-cell (val expval?) (count integer?)]
  ;; Represents a cell that is not currently holding a value
  [empty-cell]
)

;; (empty-store) returns an empty Scheme list representing the empty store.
(define empty-store (lambda () (make-vector 0)))

;; (initialize-store!) initializes the-store! to (empty-store).
(define initialize-store!
  (lambda () 
  (set! the-store! (empty-store))
  (set! free-cells! '())
  (set! store-length! 0)
  ))

;; Doubles the capacity of the store
(define double-store-capacity!* (lambda (doubledStore currentStore ref)
  (if (not (= ref (vector-length currentStore))) ;; Until we've hit the length of the current store
  ((lambda ()
    (vector-set! doubledStore ref (vector-ref currentStore ref))
    (double-store-capacity!* doubledStore currentStore (+ ref 1))
  ))
)))

(define double-store-capacity! 
(lambda () 
(let ([len (vector-length the-store!)])
  (let ([doubledStore (make-vector (if (= len 0) 1 (* len 2)) (empty-cell))]);; Construct a new store 
    (double-store-capacity!* doubledStore the-store! 0)
    (set! the-store! doubledStore)
  )
)
))

;; (newref! ev) takes an expval ev adds to the-store! and returns
;; a ref-val that points to the added value.
(define newref!
  (lambda (ev) 
  (cases expval ev
    [proc-val (params body saved-env) (increment-all saved-env)]
    [ref-val (ref) (increment_count! ev)]
    [else 'donothing ]
  )
  (cond
    [(free-cell-exists?) (let ([free-cell (pop-free-cell!)])
      (vector-set! the-store! (expval->ref free-cell) (ref-cell ev 1))
      (ref-val (expval->ref free-cell))
    )]
    [else (let ([len (vector-length the-store!)])
        (if (= len store-length!) (double-store-capacity!)) ; Double capacity if out of space
        (let ([currentIndex store-length!])
          (set! store-length! (+ store-length! 1))
          (vector-set! the-store! currentIndex (ref-cell ev 1))
          (ref-val currentIndex))
    )]
  )
))

;; (deref ev) takes an expressed value which should be a (ref-val ref)
;; and returns the value associated with ref in the-store!.
(define deref (lambda (ev) (let ([ref (expval->ref ev)]) 
  (if (< ref store-length!)
    (let ([c (vector-ref the-store! ref)])
      (cases store-cell c
        [ref-cell (val c) val]
        [empty-cell () (raise-exception 'deref "empty ref cell dereference: ~s" ref)]
      ))
    (raise-exception 'deref "empty ref cell dereference: ~s" ref)
  )
)))

;; (setref! ev1 ev2) takes two expvals, the first which should be a
;; (ref-val ref) it sets the cell ref in the-store! to ev2.  Returns (unit-val).
(define setref!
  (lambda (ev1 ev2)
    (let [[ref (expval->ref ev1)]] 
    (cond
      [(>= ref store-length!) (raise-exception 'setref! "attempted to set reference that doesn't exist: ~s" ref)]
      [else (let ([c (vector-ref the-store! ref)])
      (cases store-cell c
        [ref-cell (val count) 
          ; Current value, decrement if necessary
          (cases expval val
            [proc-val (params body saved-env) (decrement-all saved-env)]
            [ref-val (ref) (decrement_count! ev1)]
            [else 'donothing ]
          )
          ; New value, increment if necessary
          (cases expval ev2
            [proc-val (params body saved-env) (increment-all! saved-env)]
            [ref-val (ref) (increment_count! ev1)]
            [else 'donothing ]
          )
          (vector-set! the-store! ref (ref-cell ev2 count)) 
          (unit-val)
        ]
        [empty-cell () (raise-exception 'setref! "attempted to set reference that doesn't exist: ~s" ref)]
      ))]
    )
)))

;;display the store in our interpreter
(define display-store* (lambda (store ref) 
  (if (< ref (vector-length store))
    (let ([cell (vector-ref store ref)])
    (cases store-cell cell
      [ref-cell (val count) (display "(") (display (expval->string val)) (display ", ") (display (number->string count)) (display ") ")]
      [empty-cell () (display " (empty) ")]
    )
    (display-store* store (+ ref 1))
    )
  )
))

(define display-store (lambda (store) 
  (display "[ ")
  (display-store* store 0) 
  (display "]")
))