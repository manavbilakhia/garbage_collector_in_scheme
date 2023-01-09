;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  CSC 370 - Week 9
;;   Continuations
;;   continuations.scm
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,


;; ----------------------------------------------------------------
;;
;; A Gentle Introduction to Continuations: Currying 
;;
;; ----------------------------------------------------------------

;; Before we dive into continuations it can be helpful to consider a
;; related technique.  Consider the following Scheme function

(define add
  (lambda (a b)
    (+ a b)))

;; This function expects two parameters.  What if we always knew that
;; we would call this function with a = 1?  We could rewrite it as
#|
(define increment
  (lambda (b)
    (+ 1 b)))
|#
;; This method requires rewriting the function completely for each
;; different choice of a.  Functional programming and allows us to do
;; something more elegant and general.  It is a technique called
;; "Currying" (it is named after a logician and computer scientist
;; Haskell Curry was one of the founders of Programming Languages).
;; Below is a "curried" version of add.

(define add-curry
  (lambda (a)
    (lambda (b)
      (+ a b))))

;; We can now define increment simply as

(define increment (add-curry 1))

;; In general, currying a function is simply to split the function
;; into nested one parameter functions so that the parameters can be
;; supplied one at a time.  The body of the function does not change.
;; Currying works only when doing static scoping because it relies on
;; variables being captured by procedures.  Since the result of
;; applying a curried function is often another function, it is useful
;; in implementing higher order function that produce other functions.
;; Consider the function below

(define listof
  (lambda (p?)
    (lambda (ls)
      (fold-left (lambda (acc head) (and acc (p? head))) #t ls))))

;; We can use it to define new functions easily.

(define listofsymbols? (listof symbol?))
(define listoflists? (listof list?))
(define listofnumbers? (listof number?))

;; Can be used to make arithmetic operators in value-of-exp more generic.
(define make-op-value-of-exp
	(lambda (operator)
		(lambda (exp1 exp2 env)
			(let
				([ev1 (value-of-exp exp1 env)]
				 [ev2 (value-of-exp exp2 env)])
				(num-val (operator (expval->num ev1) (expval->num ev2)))))))
				
(define value-of-exp-plus-exp (make-op-value-of-exp +))
		
;; Equivalent code in value-of-exp.
;[plus-exp (exp1 exp2) (value-of-exp-plus-exp exp1 exp2 env)]
;[plus-exp (exp1 exp2) ((make-op-value-of-exp +) exp1 exp2 env)]



;; We can also generally transform non-curried function into a curried
;; function.

(define make-curried3
  (lambda (f)
    (lambda (a)
      (lambda (b)
				(lambda (c)
					(f a b c))))))


(define add-curried3 (make-curried3 +))
(define add-curried2 (add-curried3 3))
(define add-curried1 (add-curried2 4))

;; We've actually seen currying before if you think to our
;; implementation of true and false on HW 3.



;; ----------------------------------------------------------------
;;
;; Tail Recursion Recalled
;; 
;; ----------------------------------------------------------------

;; Recall the reverse functions we wrote in Week 2.

(define reverse ;; Isn't tail recursive.
  (lambda (ls)
    (cond 
     [(null? ls) '()]  ;; Base Case
     [else 
      (let ([head (car ls)]
	    [tail (cdr ls)])
        (append (reverse tail) (list head)))]))) ;; In operand position.
 
(define reverse2
  (lambda (ls)
    (reverse2* ls '())))

(define reverse2*  ;; Is tail recursive function.
  (lambda (ls acc)
    (cond
     [(null? ls) acc]  ;; Base Case
     [else
      (let ([head (car ls)]
	    [tail (cdr ls)])
        (reverse2* tail (cons head acc)))]))) ;; In tail position.

;; We saw what happened when we traced these functions.  The reverse
;; builds up control context as it recurses, i.e., it has to perform
;; an append each time reverse returns.  The second, reverse2, does
;; not, the recursive call is the last action performed by the
;; function -- such functions are called "tail recursive", and the
;; final function call is said to be a "tail call" or in "tail
;; position".

;; In the first reverse function the recursive call is in "operand
;; position" which means its result is an operand to some other
;; function (in this case append).  To evaluate this operator (append)
;; when the function returns, the original context of the function may
;; need to be known (in this case the head of ls must be remembered).

;; In reverse2, tail recursion is used, so no information about the
;; context of the recursive call must be mantained.

;; This leads us to an important design principle of programming
;; languages: Only the evaluation of operands, not function calls
;; should grow the control context.

;; ----------------------------------------------------------------
;;
;; Continuation Passing Style
;;
;; ----------------------------------------------------------------

;; Our goal is to modify an arbitrary recursive function to make it
;; tail recursive, i.e., so every call to the function is in tail
;; position. To accomplish this we will add a new parameter that
;; encapsulates control context of our computation.  This parameter is
;; called a "continuation".  It's purpose is to take the result of a
;; computation and proceed with the remainder of the computation.

;; Continuations have two interface functions:

;; end-cont : () -> Cont
;; apply-cont : Cont x Val -> FinalVal

;; The first function end-cont produces a continuation that completes
;; the computation and displays its final result.

;; The second function apply-cont takes a continuation and an value
;; and uses that value to continue on with the computation.

;; We will implement continuations as Scheme functions that maps
;; Val -> FinalVal.

;; This means the types of the interface are actually

;; end-cont : () -> (Val -> FinalVal)
;; apply-cont : (Val -> FinalVal) x Val -> FinalVal

;; Both of these functions are easily implemented in Scheme

(define end-cont
  (lambda ()
    (lambda (v)
      (printf "End of computation. ~a" v)
      (newline)
      v)))

(define apply-cont
  (lambda (cont ev)
    (cont ev)))

;; There's a general recipe to convert a recursive function to one
;; which is tail recursive using continuations.  This is called
;; "continuation passing style" (CPS).

;; 0. Rename the function to "<old-name>/cc". ("/k" is also conventional).

;; 1. Add a new parameter to function called the "current continuation".

;; 2. Modify the base cases to apply the current continuation to the
;;    result that would have been returned.

;; 3. Modify each recursive cases where the function is already in tail
;;    position by passing the current continuation to the call.

;; 4. Modify each recursive cases with the function in operand position
;;    by we constructing a new continuation to proceed with the
;;    computation after that call would have returned.

;; 5. Make a wrapper function to call the recursive function with the
;;    original input and the end continuation.

;; Pro tips:
;; + If you ever see "End of computation." printed more than once,
;;   you've messed up and called apply-cont more times than necessary.
;; + You can trace your /cc function in Scheme.  When it runs you
;;   shouldn't see the stack grow.

;; We can practice this on reverse.
(define reverse 
  (lambda (ls)
    (cond 
     [(null? ls) '()]
     [else 
      (let ([head (car ls)]
	    [tail (cdr ls)])
        (append (reverse tail) (list head)))])))












#|
(define reverse/cc ;; Rule 0.
  (lambda (ls cc) ;; Rule 1.
    (cond
     [(null? ls) (apply-cont cc '())] ;; Rule 2.
     [else
      (let ([head (car ls)]
            [tail (cdr ls)])
        (reverse/cc 
         tail
         (lambda (res)
           (apply-cont cc (append res (list head))))))]))) ;; Rule 4

(reverse/cc '(1 2 3 4) (end-cont))
|#

;; ----------------------------------------------------------------
;;
;; A Continuation Passing Interpreter
;; 
;; ----------------------------------------------------------------

;; This technique can be directly applied to make any function tail
;; recursive, even value-of-prog and value-of-exp in our interpreter.

;; For clarity, we'll define a type FinalVal = Expval.  We'll
;; specialized our interface to Expvals.

;; The interface has two functions

;; Cont = (Expval -> FinalVal)
;; end-cont : () -> Cont
;; apply-cont : Cont x Expval -> FinalVal

;; The first function end-cont produces a continuation that completes
;; the computation and displays its final result.

;; The second function apply-cont takes a continuation and an
;; expressed value and uses that value to continue on with the
;; computation.

;; Both of these functions are easily implemented in Scheme

#|
(define end-cont
  (lambda ()
    (lambda (ev)
      (printf "End of computation. ~a" (expval->string ev))
      ev)))

(define apply-cont
  (lambda (cont ev)
    (cont ev)))
|#

;; We can use the same recipe as above to convert our interpreter to
;; CPS:

;; 0. Rename our functions to end with /cc.

;; 1. Add a new parameter to value-of-exp called the "current continuation".

;; 2. If we can evaluate an expression without a recursive call we
;;    apply the current continuation to this expressed value.
;;    E.g., const-exp, const-true-exp, var-exp

;; 3. If value-of-exp already in tail position, we pass the current
;;    continuation to this call.
;;    E.g., a-prog, the body evaluations in let-exp and letrec-exp

;; 4. If value-of-exp appears in operand position, we construct a new
;;    continuation to proceed with the computation after that call to
;;    value-of-exp would have returned.
;;    E.g., zero?-exp, call-exp, diff-exp

;; 5. When the initial calls are made to value-of-prog pass (end-cont).

;; Let's practice CPS by modifying our interpreter.
