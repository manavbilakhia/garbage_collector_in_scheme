;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; -- Unit Test Framework --
;;
;; This is a simple harness for unit testing Scheme code.  The basic
;; usage is to add tests in the file you are testing via the function
;; add-my-test!, and then execute the tests after loading the file using
;; (run-all-tests!).
;;
;; Note that there is a more elaborate test harness included with the
;; course software already in the file test-harness.scm.
;;
;; Feel free to use this code or a modified version to test your
;; homework assignments.
;;
;; Author: Matthew Anderson  (F14, W17, F22)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Global variable for storing a list of tests, initially empty.
(define my-tests! '())
(define allPoints! 0)
(define allTotalPoints! 0)
(define exercises! '())

(define clear-tests! (lambda () (set! my-tests! '())))

(define add-exercise!
    (lambda [ex-name-str exercise-ls] 
        (cond 
            [(null? exercise-ls) (set! exercises! (cons ex-name-str exercises!))]
            [(not (equal? (car exercise-ls) ex-name-str)) (add-exercise! ex-name-str (cdr exercise-ls))]
        )
    )
)

;; (add-my-test! name-str qe1 qe2)
;; Function which takes a string name-str naming a test, and two
;; quoted S-expressions.  This function combines the name and the two
;; quoted expressions into a list and adds it to the head of the
;; global variable my-tests!  
;; MAKE SURE USE QUOTED EXPRESSIONS!
(define add-my-test!
  (lambda [test-name-str ex-name-str ptval qe1 qe2]
    (set! my-tests! (cons (list test-name-str ex-name-str ptval qe1 qe2) my-tests!))
    (add-exercise! ex-name-str exercises!)
   
))

(define add-batch-tests! 
    (lambda [ex-name-str test-ls] (add-batch-tests!* ex-name-str test-ls '() #f))
)

(define add-batch-tests!*
    (lambda [ex-name-str test-ls qe1 isQe2] 
        (if (not (null? test-ls)) 
            (let [[sym (car test-ls)]] (cond 
                    [(equal? sym '=>) (cond 
                        [(null? qe1) (display "'=>' used before qe1 was defined\n")]
                        [isQe2 (display "Double '=>' found after qe1\n")]
                        [else (add-batch-tests!* ex-name-str (cdr test-ls) qe1 #t)]
                    )]
                    [(null? qe1) (add-batch-tests!* ex-name-str (cdr test-ls) sym #f)]
                    [isQe2 ((lambda [] 
                        (add-my-test! "" ex-name-str 1 qe1 sym) ; Add the test 
                        (add-batch-tests!* ex-name-str (cdr test-ls) '() #f) ; Reset for next test
                    ))]
                )
            )
    ))
)

;; (display-result! val1 val2)
;; Takes two values and displays them.
(define display-result!
  (lambda (val1 val2)
    (display val1)
    (display " => ")
    (display val2)))

;; (display-test-success! name-str qe1 qe2 val1 val2)
;; Displays text to indicate test success.
(define display-test-success!
  (lambda (name-str qe1 qe2 val1 val2)
    (display name-str)
    (display " -- Success -- ")
    (display-result! qe1 qe2)
    (display "\n")
    #t
))

;; (display-test-failure! name-str qe1 qe2 val1 val2)
;; Displays text to indicate failure.
(define display-test-failure!
  (lambda (name-str qe1 qe2 val1 val2)
    (display name-str)
    (display " -- Failure\n")
    (display "  Expected: ")
    (display-result! qe1 qe2)
    (display "\n    Actual: ")
    (display-result! qe1 val1)
    (display "\n            ")
    (display-result! qe2 val2)
    (display "\n")
    #f
))

;; (run-one-test! name-str qe1 qe2)
;; Runs a test with the given name two quoted expressions
(define run-one-test!
  (lambda (name-str qe1 qe2)
    (guard
     (except [#t
	      (display name-str)
	      (display " -- Failure -- ")
	      (display-result! qe1 qe2)
	      (display " -- Exception!\n")
          #f
        ])
     (let 
	 ([val1 (eval qe1)]  ;; This is why the quote are necessary.
	  [val2 (eval qe2)])
       (cond
	[(equal? val1 val2) (display-test-success! name-str qe1 qe2 val1 val2)]
	[else (display-test-failure! name-str qe1 qe2 val1 val2)]))))
  )

;; (run-all-tests!)  
;; Runs all tests.  Note this is a 0-ary function, i.e., it takes no
;; arguments.
(define run-all-tests!
  (lambda ()
    (run-all-tests!* (reverse exercises!))
    (display "--- TOTAL: ---")
    (display "\n   Points correct:")
    (display allPoints!)
    (display "\n   Total points possible:")
    (display allTotalPoints!)
    (display "\n")
))

;; (run-all-tests!* ls)
;; Recursive function to recurse through tests running each one
(define run-all-tests!* 
  (lambda (ls)
    (if (not (null? ls))
	(let ([ex-name-str (car ls)])
	  (run-one-exercise! ex-name-str (reverse my-tests!))
      (run-all-tests!* (cdr ls))
	)))
)

(define run-one-exercise! 
    (lambda [ex-name-str q-tests] 
        (let ([result (run-one-exercise!* ex-name-str my-tests! 0 0)]) (let
            ([pts (car result)]
            [totalPts (cadr result)])
            (set! allPoints! (+ allPoints! pts)) ; Add to all points
            (set! allTotalPoints! (+ allTotalPoints! totalPts)) ; add to all total points
            (display ex-name-str)
            (display "\n   Points correct: ")
            (display pts)
            (display "\n   Total points possible: ")
            (display totalPts)
            (display "\n")
        ))
    )
)

(define run-one-exercise!* 
    (lambda [ex-name-str q-tests pts totalPts] 
        (if (not (null? q-tests))
	(let
	    [[test (car q-tests)]]
	  (let [
            [name-str (car test)] ; 1st element
		    [test-ex-name-str (cadr test)] ; 2nd element
		    [ptval (caddr test)] ; 3rd element
            [qe1 (cadddr test)] ; 4th element 
            [qe2 (cadr (cdddr test))] ; 5th element
        ] 
        (cond 
            [(equal? test-ex-name-str ex-name-str) 
                (if (run-one-test! name-str qe1 qe2)
                    (run-one-exercise!* ex-name-str (cdr q-tests) (+ pts ptval) (+ totalPts ptval)) ; On success: Add points 
                    (run-one-exercise!* ex-name-str (cdr q-tests) pts (+ totalPts ptval))); On failure: Add no points
            ]
            [else (run-one-exercise!* ex-name-str (cdr q-tests) pts totalPts)] ; Test is not part of exercise: Add no points
        )
	    ))
        (list pts totalPts) ; Return points and total points at end
)))

 (define lc1 'x)
 (define lc2 'y )
 (define lc3 '(lambda (x) x) )
 (define lc4 '(lambda (y) x) )
 (define lc5 '(x y) )
 (define lc6 '(x (y y)) )
 (define lc7 '(x (lambda (y) y)) )
 (define lc8 '(lambda (x) (x y)) )
 (define lc9 '(lambda (x) (lambda (y) x)) )
 (define lc10 '((lambda (x) x) (lambda (y) y)) )
 
 ; 1
 (add-batch-tests! "1.1" '(
     (get-lvars lc1) => '(x)
     (get-lvars lc4) => '(x)
     (get-lvars lc6) => '(x y y)
     (get-lvars lc9) => '(x)
     (get-lvars '(lambda (x) (y z))) => '(y z)
     (get-lvars '(z (lambda (x) x))) => '(z x)
     (get-lvars '((lambda (f) (lambda (x) ((g (g x)) y))) z) ) => '(g g x y z)
 ))
 
 (add-batch-tests! "1.2" '(
     (get-lparams lc1)  =>  '()
     (get-lparams lc4)  =>  '(y)
     (get-lparams lc6)  =>  '()
     (get-lparams lc9)  =>  '(x y)
     (get-lparams '(lambda (x) (y z)) )  =>  '(x)
     (get-lparams '((lambda (f) (lambda (x) ((g (g x)) y))) z) )  =>  '(f x)
     (get-lparams '(lambda (x) (lambda (y) (lambda (x) z))) )   =>  '(x y x)
 ))
 
 (add-batch-tests! "1.3" '(
     (replace-vars '(f x) )  =>  '(0 0)
     (replace-vars '(lambda (x) y) )  =>  '(lambda (x) 1)
     (replace-vars '(lambda (a) (lambda (b) c)) )  =>  '(lambda (a) (lambda (b) 2))
     (replace-vars '(a ((lambda (a) b) c)) )  =>  '(0 ((lambda (a) 1) 0))
     (replace-vars '((lambda (z) (lambda (y) ((lambda (x) (x y)) (x z)))) z)) => '((lambda (z) (lambda (y) ((lambda (x) (3 3)) (2 2)))) 0)
 ))
 
 (add-batch-tests! "1.4" '(
     (free-vars 'x )  =>  '(x)
     (free-vars '(x (y z)) )  =>  '(x y z)
     (free-vars '((lambda (x) x) y) )  =>  '(y)
     (free-vars '((lambda (z) z) (lambda (x) x)) )  =>  '()
     (free-vars '((lambda (f) (lambda (x) (f x))) y) )  =>  '(y)
     (free-vars '(lambda (x) z) )  =>  '(z)
     (free-vars '(lambda (x) (lambda (y) ((lambda (z) (x y)) z))) )  =>  '(z)
 ))

; 2
(load "hw03-2.scm")

(add-batch-tests! "2.1-2.2" '(
    (is-Zero? (zero)) => '#t
    (binary->number (zero)) => '0
    (binary->number '(1 0 0 0 1 0)) => '34
    (number->binary 0) => (zero)
    (number->binary 34) => '(1 0 0 0 1 0)
    (successor (number->binary 63)) => (number->binary 64)
    (predecessor (number->binary 63)) => (number->binary 62)
))

(add-batch-tests! "2.3" '(
    ()
))

;(add-batch-tests! "2.2" '())
;(add-batch-tests! "2.3" '())
;(add-batch-tests! "2.4" '())

(run-all-tests!)

