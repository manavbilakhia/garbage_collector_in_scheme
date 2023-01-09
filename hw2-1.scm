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
(define exercise-name-list! '())
(define total-achieved-grade 0)
(define total-achievable-grade 0)
(define clear-tests! (lambda () (set! my-tests! '())))

;; (add-my-test! name-str qe1 qe2)
;; Function which takes a string name-str naming a test, and two
;; quoted S-expressions.  This function combines the name and the two
;; quoted expressions into a list and adds it to the head of the
;; global variable my-tests!  
;; MAKE SURE USE QUOTED EXPRESSIONS!
(define add-my-test!
  (lambda (test-name-str ex-name-str ptval qe1 qe2)
    (set! my-tests! (cons (list test-name-str ex-name-str ptval qe1 qe2) my-tests!)) 
    (add-my-test!* ex-name-str exercise-name-list!)))

(define add-my-test!*
  (lambda (ex-name-str exercise-name-list1!)
    (cond
      [(not (fold-left (lambda [res ex] (or (equal? ex-name-str ex) res)) #f exercise-name-list1!)) ;; res  = previous result ex exercise
        (set! exercise-name-list! (cons ex-name-str exercise-name-list1!))]
      )))

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
    #t))

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
    #f))

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
        #f])
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
    (run-all-tests!* (reverse exercise-name-list!))))

;; (run-all-tests!* ls)
;; Recursive function to recurse through tests running each one
(define run-all-tests!* 
  (lambda (ls)
    (if (not (null? ls))
	(let
	    ([ex-name (car ls)])
      (run-one-exercise! ex-name my-tests!)
	    (run-all-tests!* (cdr ls))))))

(define run-one-exercise!
  (lambda [ex-name-str test-ls]
        (run-one-exercise!* ex-name-str test-ls 0 0)))
  
  (define run-one-exercise!*
    (lambda [ex-name-str test-ls achieved-pts total-pts]
    (if (not (null? test-ls))
    (let 
      ([current-test (car test-ls)])
        (let
          ([exercise-name (cadr current-test)]
           [pts (caddr current-test)]
           [qe1 (cadddr current-test)]
           [qe2 (cadr (cdddr current-test))])
           (if (equal? exercise-name ex-name-str)
            ((lambda [] (set! total-achievable-grade (+ total-achievable-grade 1))
            (if (run-one-test! (car current-test) qe1 qe2) 
              ((lambda [] (set! total-achieved-grade (+ total-achieved-grade 1))
              (run-one-exercise!* ex-name-str (cdr test-ls) (+ pts achieved-pts) (+ total-pts pts))))
            (run-one-exercise!* ex-name-str (cdr test-ls) achieved-pts (+ total-pts pts))))) ;; runs the next test with the same exercise if the previous one fails
            (run-one-exercise!* ex-name-str (cdr test-ls) achieved-pts total-pts)) ;; runs the next exercise 
            ))
    (display-pts! total-pts achieved-pts))
              ))

(define display-pts!
  (lambda [total-pts pts]
    ;;(display "achievable points in this exercise ")
    ;;(display total-pts)
    ;;(display "\n")
    ;;(display "total points achieved in this exercise ")
    ;;(display pts)
    ;;(display "\n")
    (display "total points achievable in this assignment ")
    (display total-achievable-grade)
    (display "\n")
    (display "total points achieved in this assignment ")
    (display total-achieved-grade)
    ))
            
(define add-batch-tests!
  (lambda [ex-name-str q-tests]
    (if (not (null? q-tests))
      (let
        ([qe1 (car q-tests)]
         [qe2 (caddr q-tests)])
         (add-my-test! "" ex-name-str 1 qe1 qe2)
         (add-batch-tests! ex-name-str (cdddr q-tests))))))

  
;; Sample tests for functions we wrote above
;;(add-my-test! "Reverse test" "EX1" 1 '(reverse '(1 2 3)) ''(3 2 1))
;(add-my-test! "Fib test" '(fib 4) '3)
;(add-my-test! "Fib test *SHOULD FAIL*" '(fib 5) ''(1 3 4)) ;; should fail
;(add-my-test! "numMonth->strMonth" "EX1" 1 '(numMonth->strMonth 9) '"September")
;(add-my-test! "numMonth->strMonth" "EX1" 1 '(numMonth->strMonth 1) '"October")


(add-batch-tests! "Exercise 1 (2pts)" '(
(times10 '(1 2 3 4 5))  =>  '(10 20 30 40 50)
(times10 '(25))  =>  '(250)
))

(add-batch-tests! "Exercise 2 (2pts)" '(
(pair-up 'x '(a b c d))  =>  '((x . a) (x . b) (x . c) (x . d))
(pair-up 'a '(1 2 3))  =>  '((a . 1) (a . 2) (a . 3))
(pair-up 'a '())  =>  '() 
))

(add-batch-tests! "Exercise 4 (2pts)" '(
(replace 'red 'blue '(red fish blue fish))  =>  '(blue fish blue fish)
(replace 'spam 'ham '(green eggs and ham))  =>  '(green eggs and ham)
(replace 'ham 'spam '(green eggs and ham))  =>  '(green eggs and spam) 
))

(add-batch-tests! "Exercise 5 (2pts)" '(
(remove 'cream '(i scream for ice cream))  =>  '(i scream for ice)
(remove 'scream '(i scream you scream we all scream))  =>  '(i you we all)
))

(add-batch-tests! "Exercise 7 (2pts)" '(
(length '(1 2 3)) => 3
(length '((1 2 3)(4 5 6))) => 2
(length '(1 2 3 (4 5 6))) => 4
))

(add-batch-tests! "Exercise 8 (2pts)" '(
(average '(1 2 3)) => 2
(average '(1 0 -1)) => 0
(average '(4 5 6 7)) => 11/2
))

(add-batch-tests! "Exercise 10 (2pts)" '(
(reverse '(1 2 3)) => '(3 2 1)
(reverse '((1 2)(3 4))) => '((3 4)(1 2))
(reverse '("hello" "world")) => '("world" "hello")
))










