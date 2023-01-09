;; hw5-soln.scm
;; Fall 2022

(load "helpers.scm")

;; ================ Parser Definitions ==================================

;; This defines the translation from the concrete syntax to the abstract syntax
(define the-grammar
  '((program                        ;; <Program> ::= 
     (expression)                   ;;   Concrete    <Expression>
     a-prog)                        ;;   Abstract    (a-prog exp)
    
    (expression                     ;; <Expression> ::= 
     (number)                       ;;   Concrete       <Number> 
     const-exp)                     ;;   Abstract       (const-exp num)

    ;; ============== LET =================
      
    (expression                     ;; <Expression> ::= 
     ("zero?(" expression ")")      ;;   Concrete       zero?(<Expression>)
     zero?-exp)                     ;;   Abstract       (zero?-exp exp)
    
    (expression                                             ;; <Expression> ::= 
     ("if" expression "then" expression "else" expression)  ;;   Concrete       if <Expression> then <Expression> else <Expression>
     if-exp)                                                ;;   Abstract       (if-exp exp1 exp2 exp3)
        
    (expression                     ;; <Expression> ::= 
     (identifier)                   ;;   Concrete       <Identifier>
     var-exp)                       ;;   Abstract       (var-exp var)
    
    (expression                                          ;; <Expression> ::= 
     ("let" identifier "=" expression "in" expression)   ;;   Concrete       let <Identifier> = <Expression> in <Expression>
     let-exp)                                            ;;   Abstract       (let-exp var exp1 exp2)

     ;; ============== PROC ==================
    
    (expression                                        ;; <expression> ::=
     ("proc(" identifier ")" expression)              ;;   Concrete  proc (<identifier>) <expression>
     proc-exp)                                         ;;   Abstract  (proc-exp param body)
    
    (expression                                        ;; <expression> ::=
     ("(" expression expression ")")                   ;;   Concrete  (<expression> <expression>)
     call-exp)                                         ;;   Abstract  (call-exp rator rand)

    ;; ============== LETREC =================
    
    (expression                                                            ;; <expression> ::=
     ("letrec" identifier "(" identifier ") =" expression "in" expression) ;;   letrec <id> (<id>) = <exp> in <exp>
     letrec-exp)                                                           ;;   (letrec-exp f-name f-param f-body body)

    ;; ============== HW 5 =================

    (program                               ;; <Program> ::= 
     ("def!" identifier "=" expression)    ;;  Concrete     def! <Identifier> = <Expression>
     def-prog)                             ;;  Abstract     (def-prog var exp)
    
    (expression                            ;; <Expression> ::= 
     ("#true")                             ;;   Concrete       #true
     const-true-exp)                       ;;   Abstract       (const-true-exp)
    
    (expression                            ;; <Expression> ::=
     ("#false")                            ;;   Concrete       #false
     const-false-exp)                      ;;   Abstract       (const-false-exp)
     
    (expression                            ;; <Expression> ::= 
     ("*(" expression "," expression ")")  ;;   Concrete       *(<Expression>,<Expression>)
     times-exp)                            ;;   Abstract       (times-exp exp1 exp2)
    
    (expression                            ;; <Expression> ::= 
     ("/(" expression "," expression ")")  ;;   Concrete       /(<Expression>,<Expression>)
     div-exp)                              ;;   Abstract       (div-exp exp1 exp2)
    
    (expression                            ;; <Expression> ::= 
     ("-(" expression "," expression ")")  ;;   Concrete       -(<Expression>,<Expression>)
     diff-exp)                             ;;   Abstract       (diff-exp exp1 exp2)
    
    (expression                            ;; <Expression> ::= 
     ("+(" expression "," expression ")")  ;;   Concrete       +(<Expression>,<Expression>)
     plus-exp)                             ;;   Abstract       (plus-exp exp1 exp2)
    
    (expression                            ;; <Expression> ::= 
     ("=(" expression "," expression ")")  ;;   Concrete       =(<Expression>,<Expression>)
     equal-exp)                            ;;   Abstract       (equal-exp exp1 exp2)

    (expression                            ;; <Expression> ::= 
     ("<(" expression "," expression ")")  ;;   Concrete       <(<Expression>,<Expression>)
     less-than-exp)                        ;;   Abstract       (less-than-exp exp1 exp2)
    
    (expression                            ;; <Expression> ::= 
     ("&(" expression "," expression ")")  ;;   Concrete       &(<Expression>,<Expression>)
     and-exp)                              ;;   Abstract       (and-exp exp1 exp2)

    (expression                            ;; <Expression> ::= 
     ("|(" expression "," expression ")")  ;;   Concrete       |(<Expression>,<Expression>)
     or-exp)                               ;;   Abstract       (or-exp exp1 exp2)
    
    (expression                            ;; <Expression> ::= 
     ("!(" expression ")")                 ;;   Concrete       !(<Expression>)
     not-exp)                              ;;   Abstract       (not-exp exp)

    (expression                               ;; <Expression> ::=
     ("cons(" expression "," expression ")")  ;;   Concrete       cons(<Expression>,<Expression>)
     cons-exp)                                ;;   Abstract       (cons-exp exp1 exp2)
    
    (expression                            ;; <Expression> ::=
     ("car(" expression ")")               ;;   Concrete       car(<Expression>)
     car-exp)                              ;;   Abstract       (car-exp exp)
 
    (expression                            ;; <Expression> ::=
     ("cdr(" expression ")")               ;;   Concrete       cdr(<Expression>)
     cdr-exp)                              ;;   Abstract       (cdr-exp exp)
 
    (expression                            ;; <Expression> ::=
     ("null?(" expression ")")             ;;   Concrete       null?(<Expression>)
     null?-exp)                            ;;   Abstract       (null?-exp exp)
 
    (expression                            ;; <Expression> ::=
     ("emptylist")                         ;;   Concrete       emptylist
     emptylist-exp)                       ;;   Abstract       (emptylist-exp)

     ;; ============= Explicit References ===============

  (expression
    ("newref!(" expression ")")
    newref!-exp)

  (expression
    ("deref(" expression ")")
    deref-exp)

  (expression
    ("setref!(" expression "," expression ")")
    setref!-exp)

     ;; ============= Implicit References ===============
     (expression
       ("set!" identifier "=" expression)
       set!-exp)

     
  ))

(load "lex-scan-parse.scm")

;; =============== Environment Definition =============================


;; This is an implementation of the var-val pair list representation
;; of an environment, we wrote earlier.  I translated the
;; representation into a define-datatype so we get the constructors
;; and type checking predicate for free, and can use cases to process.

(define-datatype environ environ?
  (empty-env)                   ;; (empty-env) gives an empty environment
  (extend-env                   ;; (extend-env var val env) extends the environment
   (var symbol?)
   (val ref-val?)
   (env environ?))
  (extend-env-rec
   (p-name symbol?)
   (p-param symbol?)
   (p-body expression?)
   (saved-env environ?))
  )

;; (apply-env env target-var) s to figure out the maping of target-var
;; in the environment env.
(define apply-env ; Env x Var -> Expval
  (lambda (env target-var)
    (cases environ env
      [extend-env (var val env)
        (if (equal? var target-var)
          val
          (apply-env env target-var))]
      [empty-env () (raise-exception 'apply-env "No binding for ~s" target-var)]
      [extend-env-rec [p-name p-param p-body saved-env]
        (if (equal? p-name target-var)
          (newref! (proc-val p-param p-body
            (extend-env-rec p-name p-param p-body saved-env)))
          (apply-env saved-env target-var))]
      )))

(define make-init-env
  (lambda ()
    (extend-env 
     'pi (newref! (num-val 3.14159))
     (extend-env
      'e (newref! (num-val 2.71828))
      (empty-env)))))

(define env->string
  (lambda (env)
    (cases environ env
      [empty-env () "[]"]
      [extend-env (var val env*)
        (string-append "[" (symbol->string var) 
		       " = "  (expval->string (deref val)) 
		       (env->string* env*) "]")]
      [extend-env-rec (p-name p-param p-body saved-env)
                      (string-append "[" (symbol->string p-name) 
                                     "(" (symbol->string p-param) ")" 
                                     (env->string* env*) "]")])))

(define env->string*
  (lambda (env)
    (cases environ env
      [empty-env () ""]
      [extend-env (var val env*) 
        (string-append ", " (symbol->string var) 
          " = " (expval->string (deref val)) 
          (env->string* env*))]
      [extend-env-rec (p-name p-param p-body saved-env)
        (string-append ", " (symbol->string p-name) "(" (symbol->string p-param) ")"
          (env->string* env*))])))

;; ==================== Expressed Values ==================================

;; Expressed values are Int + Bool + Proc + Unit + List(Expvals) + Emptylist
(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?))
  (proc-val
   (param symbol?)
   (body expression?)
   (saved-env environ?))
  (unit-val)
  (list-val
   (ls (list-of expval?)))
  (emptylist-val)  ;; Extra value to distinguish pairs and lists.
  (a-thunk
    (exp expression?)
    (env environ?))
  (ref-val
    (ref integer?))
)

(define ref-val?
  (lambda (v)
    (and (expval? v)
      (cases expval v
        [ref-val (r) #t]
        [else #f]))))

(define expval->num 
  (lambda (ev)
    (cases expval ev
      [num-val (num) num]
      [bool-val (bool) (if bool 1 0)]
      [else (raise-exception 'expval->num "Expressed value is not a number or a Boolean: ~s" ev)])))

(define expval->bool
  (lambda (ev)
    (cases expval ev
      [bool-val (bool) bool]
      [num-val (num) (not (= num 0))]
      [else (raise-exception 'expval->bool "Expressed value is not a Boolean or a number: ~s" ev)])))

(define expval->ref
  (lambda (ev)
    (cases expval ev
      [ref-val (ref) ref]
      [else (raise-exception 'expval->list "Expressed value is not a ref: ~s" ev)])))

(define expval->list
  (lambda (ev)
    (cases expval ev
      [list-val (ls) ls]
      [else (raise-exception 'expval->list "Expressed value is not a list: ~s" ev)])))

(define expval->string
  (lambda (ev)
    (cases expval ev
           [bool-val (bool) (if bool "#true" "#false")]
           [num-val (num) (number->string num)]
           [proc-val (param body saved-env)
                     (string-append "#proc(" (symbol->string param) ")")] 
           [unit-val () "#void"]
           [list-val (ls)  ;; Clunky attempt to mirror scheme list / pair display.
                     (let
                         [[last (list-ref ls (- (length ls) 1))]]
                       (let
                           [[s
                             (fold-left
                              string-append
                               ""
                               (map
                                 (lambda (ev) (string-append (expval->string ev) " "))
                                 (reverse (cdr (reverse ls)))))]]
                         (cases expval last
                           [emptylist-val ()
                             (string-append "("
                               (if (> (string-length s) 0)
                                 (string-truncate! s (- (string-length s) 1))
                                 "")
                               ")")]
                           [else (string-append "(" s ". " (expval->string last) ")")])))]
      [emptylist-val () "()"]
      [ref-val (ref) (string-append "ref(" (number->string ref) ")")]
      [a-thunk (exp env) "a-thunk"]
      )))

      
(load "store.scm")

;; ==================== Continuations ================================

;; Cont = Expval -> FinalExpval

(define end-cont ;; () -> Cont
  (lambda ()
    (lambda (ev)
      (printf "End of computation ~a." (expval->string ev))
      (newline)
      ev)))

(define apply-cont  ;; Cont x Expval -> FinalExpval
  (lambda (cont ev)
    (cont ev)))

;; ==================== Evaluater ====================================

;; Curried evaluation of operators.
(define make-eval-op
  (lambda (arg-> result-cons)
    (lambda (op exp1 exp2 env)
      (result-cons (op (arg-> (value-of-exp exp1 env)) (arg-> (value-of-exp exp2 env))))
      )))


;; Specialized for arithmetic operators and comparions.
(define eval-bin-arith (make-eval-op expval->num num-val))
(define eval-bin-comp (make-eval-op expval->num bool-val))
;; Doesn't work for and / or because they aren't functions in scheme.

(define value-of/cc ;; Rule 0.
  (lambda (prog env cc)  ;; Rule 1.
    (cases program prog
      [a-prog (exp) (value-of-exp/cc exp env cc)] ;; Rule 3.
#|
      [def-prog (var exp)
        (cons (unit-val) (extend-env var (newref! (value-of-exp exp env)) env))]
|#
      [else (raise-exception 'value-of-prog "Abstract syntax case not implemented: ~s" (car prog))])))

(define value-of-exp/cc ;; Rule 0.
  (lambda (exp env cc)  ;; Rule 1.
    (cases expression exp
      ;; ======== STARTER ==========
      [const-exp (num) (apply-cont cc (num-val num))] ;; Rule 2.
      [zero?-exp (exp1)
        (value-of-exp/cc exp1 env  ;; Rule 4.
          (lambda (ev)
            (apply-cont cc (bool-val (= (expval->num ev) 0)))))]  ;; Rule 2.

      [div-exp (exp1 exp2)
        (value-of-exp/cc exp1 env
          (lambda (ev1)
            (let [[val1 (expval->num ev1)]]
              (value-of-exp/cc exp2 env
                (lambda (ev2)
                  (let [[val2 (expval->num ev2)]]
                    (if (= val2 0)
                      (raise-exception 'value-of-exp "Divide by zero exp = ~s with env = ~s" exp env)
                      (apply-cont cc (num-val (/ val1 val2))))) )))))]
      
      
      [if-exp (exp1 exp2 exp3)
        (value-of-exp/cc exp1 env  ;; Rule 4.
          (lambda (ev)
            (if (expval->bool ev)
              (value-of-exp/cc exp2 env cc) (value-of-exp/cc exp3 env cc))))] ;; Rule 3 & 3.
      [proc-exp [param body] (apply-cont cc (proc-val param body env))] ;; Rule 2.
      [var-exp (var) (apply-cont cc (deref (apply-env env var)))] ;; Rule 2.      
      [call-exp [rator rand]
        (value-of-exp/cc rator env  ;; Rule 4.
          (lambda (ev1)
            (cases expval ev1
              [proc-val [param body saved-env]
                (cases expression rand
                  ;; Rule 3.
                  [var-exp (var)
                    (value-of-exp/cc body (extend-env param (apply-env env var) saved-env) cc)]
                  [else
                    ;; Rule 4.
                    (value-of-exp/cc rand env
                      (lambda (ev2)
                        ;; Rule 3.
                        (value-of-exp/cc body (extend-env param (newref! ev2) saved-env) cc)))])]
              [else
                (raise-exception
                  'value-of-exp
                  "Attempted to call non-procedure: ~s" rator)])))]

      
#|
      ;; ========== LET ===========
      [diff-exp (exp1 exp2) (eval-bin-arith - exp1 exp2 env)]

      [let-exp (var exp1 exp2)
        (value-of-exp exp2 (extend-env var (newref! (value-of-exp exp1 env)) env))]

      ;; ========== PROC ==========




      
      ;; ========= LETREC =========
      [letrec-exp [p-name p-param p-body body]
        (value-of-exp body (extend-env-rec p-name p-param p-body env))]

      
      ;; ========== HW 5 ==========
      [const-true-exp () (bool-val #t)]
      [const-false-exp () (bool-val #f)]
      [plus-exp (exp1 exp2) (eval-bin-arith + exp1 exp2 env)]

      [times-exp (exp1 exp2) (eval-bin-arith * exp1 exp2 env)]
      [less-than-exp (exp1 exp2) (eval-bin-comp < exp1 exp2 env)]
      [equal-exp (exp1 exp2) (eval-bin-comp = exp1 exp2 env)]
      [and-exp (exp1 exp2) (bool-val (and (expval->bool (value-of-exp exp1 env)) (expval->bool (value-of-exp exp2 env))))]
      [or-exp (exp1 exp2) (bool-val (or (expval->bool (value-of-exp exp1 env)) (expval->bool (value-of-exp exp2 env))))]
      [not-exp (exp1) (bool-val (not (expval->bool (value-of-exp exp1 env))))]

      ;; === HW 5 - Extra Credit ===
      ;; Implementation is a little clunky.
      [cons-exp (exp1 exp2)
                (let
                    [[ev1 (value-of-exp exp1 env)]
                     [ev2 (value-of-exp exp2 env)]]
                  (cases expval ev2
                        [list-val (lst) (list-val (cons ev1 (expval->list ev2)))]
                        [else (list-val (list ev1 ev2))]))]
      [car-exp (exp1)
	       (let
		   [[ls (expval->list (value-of-exp exp1 env))]]
                 (cond
                  [(null? ls) (raise-exception 'value-of-exp "Attempting to car empty list." exp env)]
                  [else
                   (cases expval (car ls)
                          [emptylist-val () (raise-exception 'value-of-exp "Attempting to car empty list." exp env)]
                          [else (car ls)])]))]
      [cdr-exp (exp1)
	       (let
		   [[ls (expval->list (value-of-exp exp1 env))]]
                 (cond
                  [(null? ls) (raise-exception 'value-of-exp "Attempting to cdr empty list." exp env)]
                  [(= (length ls) 1) (raise-exception 'value-of-exp "Attempting to cdr empty list." exp env)]
                  [(= (length ls) 2)
                   (cases expval (cadr ls)
                          [emptylist-val () (list-val (list (emptylist-val)))]
                          [else (cadr ls)])]
                  [else (list-val (cdr ls))]))]
      [null?-exp (exp1) (bool-val (equal? (list (emptylist-val)) (expval->list (value-of-exp exp1 env))))]
      [emptylist-exp () (list-val (list (emptylist-val)))]

      ;; ======== REF ==============
      [newref!-exp (exp)  (newref! (value-of-exp exp env))]
      [deref-exp (exp)  (deref (value-of-exp exp env))]
      [setref!-exp (exp1 exp2) (setref! (value-of-exp exp1 env) (value-of-exp exp2 env))]

      ;; ======== IMP ==============
      [set!-exp (var exp) (setref! (apply-env env var) (value-of-exp exp env)) (unit-val)]

  |#    
      [else (raise-exception 'value-of-exp "Abstract syntax case not implemented: ~s" (car exp))])))


;; ==================== Interpreter ====================================

;; (start) -- Starts the interpreter.
(define start
  (lambda ()
    (begin
      (display "\n=== Welcome to the HW 5 Interpreter === \n\n")
      (initialize-store!)
      (read-eval-print (make-init-env)))))

;; (read-eval-print) -- Main read, eval, and print loop.
(define read-eval-print
  (lambda (env)
    ;; Display an interpreter prompt
    ;;(display "==> ")
    ;; Read a line user input
    (let [[code (get-input-string)]]
      (cond 
       [(equal? code "!quit")
	(display "Goodbye!")  ;; Quit if 'quit entered.
	(newline)]
       [else   ;; Do something
	(cond
	 [(equal? code "!debug0")
	  (untrace value-of value-of-exp)
	  (untrace expval->num expval->bool expval->string)]
	 [(equal? code "!debug1")
	  (trace value-of value-of-exp)
	  (untrace expval->num expval->bool expval->string)]
	 [(equal? code "!debug2")
	  (trace value-of value-of-exp expval->num expval->bool expval->string)]
	 [(equal? code "!debug3")
	  (trace value-of value-of-exp expval->num expval->bool expval->string apply-env extend-env empty-env)]
	 [(equal? code "!env")
	  (display (env->string env))
	  (newline)]
	 [(equal? code "!reset-env")
	  (set! env (make-init-env))]
	 [else
	  ;; Parse code, eval expression, and print result.
	  (guard  ;; Catches parse exceptions from sllgen
	   (parse-except 
	    [else    ;; With only an else case this catches any every exception.
	     (display "Parse error:\n")
	     (display-exception parse-except)
	     ])
	   (let
	       [[abstract-code (parse code)]]  ;; Try to parse the input line
	     (guard   ;; Catches runtime exceptions from value-of
	      (value-of-except 
	       [else
		(display "Runtime error:\n")
		(display-exception value-of-except)
		])
	      (let*
		  [[result (value-of/cc abstract-code env (end-cont))] ;; Rule 5.
                   [val result]
                   ;;[val (car result)]
                   ;; [new-env (cdr result)]]
        [new-env env]]
		(display (expval->string val))
		(set! env new-env)  
		(newline)
		))))])
	(read-eval-print env)]))))





