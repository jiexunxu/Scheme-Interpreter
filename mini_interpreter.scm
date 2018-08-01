;
; Mini-Scheme Interpreter
;

(define (repl)
	(begin     ;;; the read-eval-print loop.
  	(display ">> ") 
  	(letrec ((exp (read)))
   	 	(cond ((equal? exp '(exit))      ; (exit) only allowed at top level
	 	 	'done)
	 	 	(else (display (top-eval exp))
		 		(newline)
		 		(repl))
))))
	  
(define (my-load filename)       ;; don't want to redefine the Scheme LOAD
  (load-repl (open-input-file filename)))


(define (load-repl port)
  (letrec ((exp (read port)))
    (cond ((eof-object? exp) 'done)
	  (else (letrec ((res (top-eval exp)))
		  (display res)
		  (load-repl port)))
	  )))

;; (define ....) is only allowed at the top level and affects only the 
;; global environment. Only the basic form of define is supported here.
(define (top-eval exp)
  (cond 
  	((not (pair? exp)) (eval exp *global-env*))
		((eq? (car exp) 'define)         
			(cond
				((not (pair? (cadr exp)))      
	 				(set-cdr! *global-env* (cons (car *global-env*) (cdr *global-env*)))
	 				(set-car! *global-env* (list (cadr exp) (eval (caddr exp) *global-env*)))
	 				(cadr exp)
	 			)
	 			(else (handle-function-syntax exp))
	 		)
	 	)
	 	((eq? (car exp) 'define-macro)
	 		(handle-macro-syntax exp)
	 	)
		(else (eval exp *global-env*))		
	)
)

; handles the syntax for defining a function. Syntax for (define (f x ..) (..)) <-> (define f ((lambda x ..) (..))). Reconstruct the input exp and treat f as a variable
(define (handle-function-syntax exp)
	(letrec ((args (list (cdr (cadr exp)))) ;arguments of the function, args in the form ((x ..))
				 (body (cddr exp)) ;function body, body in the form ((..))
				 (new_body (list (cons 'lambda (append args body)))) ;new_body is ( (lambda (x ..) (..)) )
				 (symbol (car (list (cadr exp))))
				 (exp2 (append (list 'define (car symbol)) new_body)) )
	 	(set-cdr! *global-env* (cons (car *global-env*) (cdr *global-env*)))
	 	(set-car! *global-env* (list (cadr exp2) (eval (caddr exp2) *global-env*)))
	 	(cadr exp2)
	)
)

; handles the syntax for defining a macro. Syntax for (define-macro (f x ..) (..)) <-> (define-macro f ((lambda x ..) (..))). Reconstruct the input exp and treat f as a variable
(define (handle-macro-syntax exp)
	(letrec ( (args (list (cdr (cadr exp)))) ;arguments of the function, args in the form ((x ..))
				 (body (cddr exp)) ;function body, body in the form ((..))
				 (new_body (list (cons 'lambda-macro (append args body)))) ;new_body is ( (lambda-macro (x ..) (..)) )
				 (symbol (car (list (cadr exp))))
				 (exp2 (append (list 'define (car symbol)) new_body)) ) ;exp2 is (define f new_body)
	 	(set-cdr! *global-env* (cons (car *global-env*) (cdr *global-env*)))
	 	(set-car! *global-env* (list (cadr exp2) (eval (caddr exp2) *global-env*)))
	 	(cadr exp2)
	)
)

(define (lookup var env)
(begin 
  (letrec ((item (assoc var env)))  		  	
    (cond ((null? item) (display "Error: Undefined Symbol ")
			(display var)
			(error ""))
	  (else  (cadr item))
	)
)))

(define (handle-if test then-exp else-exp env)
  (if (eval test env)
      (eval then-exp env)
      (eval else-exp env)))

;; function to handle call for cond. 
(define (handle-cond exp env)
	(if (null? exp) (display "") ;Return #void if no branch is true and no "else" branch    	
		(letrec ((first-exp (car exp)))
			(if (eq? (car first-exp) 'else)
				(handle-begin (cdr first-exp) env)     
				(if (eval (car first-exp) env)
	  	  	(handle-begin (cdr first-exp) env)
	    		(handle-cond (cdr exp) env)
	    	)
	  	)
		)
	)
)

;; function to handle call for let. Assume input expression exp is (let ((x 3) (y (+ 1 2) ..) (..)). First evaluate the expressions in the scope of let, and then use "cons" to put these temporary variables x, y ... into the env and use the function eval to evaluate the body (..) using the new environment
(define (handle-let exp env)
	(letrec ( (args (car exp)) (body (cdr exp)) (old_env (list-copy env)) (new_env (list-copy env)) ) ;args is ((x 3) (y (+ 1 2) ..), body is (..)
		(eval-args-in-let args old_env new_env)
		(handle-begin body new_env)
	)
)

;; evaluate the values in the args if any arg is a expression. Args is ((x 3) (y (+ 1 2) ..), so will evaluate the arg (+ 1 2) using the constant environment env
(define (eval-args-in-let args old_env new_env)
	(cond 
		((eq? args '()) '())
		(else	  
			(letrec ( (top-arg (car args)) (symbol (car top-arg)) ) 
				(cond 
					((pair? (cadr top-arg))	
						(cond ((eq? (car (cadr top-arg)) 'lambda) 
							(error "defining recursive function using let is not allowed"))					
				  		(else 
				  			(set-cdr! new_env (cons (car new_env) (cdr new_env)))
	 							(set-car! new_env (list symbol (eval (cadr top-arg) old_env)))
	 						)	
	 					)			  
					)
					(else
						(set-cdr! new_env (cons (car new_env) (cdr new_env)))
	 					(set-car! new_env (list symbol (eval (cadr top-arg) old_env)))
	 				)	
	 			)	
				(eval-args-in-let (cdr args) old_env new_env)
			)
		)
	)
)

;; function to handle call for let*. Same as function to handle let
(define (handle-let* exp env)
	(letrec ( (args (car exp)) (body (cdr exp)) (new_env (list-copy env)) ) ;args is ((x 3) (y (+ x 2) ..), body is (..)	
	  (eval-args-in-let* args new_env)	  
		(handle-begin body new_env)
	)
)

;; Almost same as the function to eval args in let, except that previosly defined symbols in last arg is appended to the environment
(define (eval-args-in-let* args env)
	(cond 
		((eq? args '()) '())	
		(else	  
			(letrec ( (top-arg (car args)) (symbol (car top-arg)) )				
				(cond 
					((pair? (cadr top-arg))
						(cond ((eq? (car (cadr top-arg)) 'lambda) 
							(error "defining recursive function using let is not allowed"))						
				  		(else 
				  			(set-cdr! env (cons (car env) (cdr env)))
	 							(set-car! env (list symbol (eval (cadr top-arg) env)))
	 						)	
	 					)					
				  ;	(set-cdr! env (cons (car env) (cdr env)))
	 				;	(set-car! env (list symbol (eval (cadr top-arg) env)))				  
					)
					(else
						(set-cdr! env (cons (car env) (cdr env)))
	 					(set-car! env (list symbol (eval (cadr top-arg) env)))
	 				)	
	 			)	
				(eval-args-in-let* (cdr args) env)
			)
		)
	)
)

;; function to handle call for letrec. Assume input exp is (letrec ((x 3) (y (+ x 1)) (z (lambda (..) (..))) ..) (..)). 
(define (handle-letrec exp env)
	(letrec ( (args (car exp)) (body (cdr exp)) (new_env (list-copy env)) ) 
		(eval-args-in-letrec args new_env)
		(handle-begin body new_env)
	)
)

;; Almost same as the function to eval args in let*, except that when m is a list in (x m), consider the case that m may be a function of the form (lambda (..) (..))
;Args is ((x 3) (y (+ 1 2) ..), so the function will evaluate the arg (+ 1 2) using the constant environment env
(define (eval-args-in-letrec args env)
	(cond 
		((eq? args '()) '())
		(else	  
			(letrec ( (top-arg (car args)) (symbol (car top-arg)) ) 
				(cond 
					((pair? (cadr top-arg))							  	
				  	(set-cdr! env (cons (car env) (cdr env)))
	 					(set-car! env (list symbol (eval (cadr top-arg) env)))				  
					)
					(else						
						(set-cdr! env (cons (car env) (cdr env)))
	 					(set-car! env (list symbol (eval (cadr top-arg) env)))
	 				)	
	 			)	
				(eval-args-in-letrec (cdr args) env)
			)
		)
	)
)

;; function to handle call for begin. returns the last expression evaluated. Assumes that exp is a list of expressions. Assumes that exp has length>=1
(define (handle-begin exp env)
	(letrec ((result (eval (car exp) env)))
		(if (not (null? (cdr exp) )) (handle-begin (cdr exp) env) result)
	)
)		

(define (eval exp env)
  (cond
   ((symbol? exp) (lookup exp env))
   ((not (pair? exp)) exp)
   ((eq? (car exp) 'quote) (cadr exp))
   ((eq? (car exp) 'if)
    (handle-if (cadr exp) (caddr exp) (cadddr exp) env))
   ((eq? (car exp) 'lambda)
    (list 'closure exp env))
   ((eq? (car exp) 'lambda-macro)
    (list 'closure-macro exp env))
   ((eq? (car exp) 'cond) (handle-cond (cdr exp) env))
   ((eq? (car exp) 'begin) (handle-begin (cdr exp) env))
   ((eq? (car exp) 'let) (handle-let (cdr exp) env))
   ((eq? (car exp) 'let*) (handle-let* (cdr exp) env))
   ((eq? (car exp) 'letrec) (handle-letrec (cdr exp) env))
   (else
		(letrec ( (symbol (lookup (car exp) env)) )
			(cond 
			  ( (eq? (car symbol) 'closure-macro) (handle-macro-call exp (cadr symbol) (caddr symbol)) )
				(else (handle-call (map (lambda (sub-exp) (eval sub-exp env)) exp)))
			)
		)
   )
))
	
	
;; formals are (x y z ...), actuals are (1 2 (+ 1 2) ..)
(define (bind formals actuals)
  (cond ((null? formals) '())
	(else (cons (list (car formals) (car actuals))
		    (bind (cdr formals) (cdr actuals))))
	))


(define (handle-block block env)
  (cond ((null? block) (error "Can't have empty block or body"))
	((null? (cdr block)) (eval (car block) env))
	(else (eval (car block) env)
	      (handle-block (cdr block) env))
	))
    

(define (handle-call evald-exps)
  (letrec ( (fn (car evald-exps)) (args (cdr evald-exps)) )	  	
   (cond
   	 ((eq? (car fn) 'closure)
       (letrec ( (formals (cadr (cadr fn))) (body (cddr (cadr fn))) (env (caddr fn)) )
          (handle-block body (append (bind formals args) env))
       )  
     ) 
     ((eq? (car fn) 'primitive-function) (apply (cadr fn) args))
   (else (error "Calling non-function"))
)))

; handles the call for a macro function f. Assume f has already been put in env
; the call is in the form (f m (+ 1 2) ..), where f=(closure-macro ...)
(define (handle-macro-call exp f env)
	(letrec ( (body (caddr f)) (formals (cadr f)) (args (cdr exp)) (arg (bind formals args)) (temp (replace-all body arg)) )	  	  
		(eval temp env)		
	)
)

; args are of the form ((x (+ 1 2)) (y 3) (z (+ m 3) ..), body is the function body.
; In the body part, replace every occurance of the first symbol in each entry of args with the second symbol 
(define (replace-all body args)
  (letrec ((temp body))
    (cond
      ((eq? (cdr args) '()) (replace body (car args)))           	             
      (else              
        (replace-all (replace body (car args)) (cdr args))       
      )
    )
  )
)

; body is function body, arg is (x y). Replace every occurrance of x in body with y
(define (replace body arg)
  (cond
    ((eq? body '()) '())
    ((not (pair? body)) (if (eq? body (car arg)) (list (cadr arg))  (list body))) 
    (else
    	(letrec ( (temp1 (replace (car body) arg)) (temp2 (replace (cdr body) arg)) )
      	(if (pair? (car body)) (append (list temp1) temp2) (append temp1 temp2))
      )	
    )
  )
)

; The specialized version of the apply function
(define (my-apply fn args)
	(if (eq? (car fn) 'primitive-function)	
		(handle-call (cons fn args))					
		(if (not (pair? args)) 
			(handle-call (cons fn (list args)))
			(handle-call (cons fn args))
		)	
	)				
)

; Return a copy of a list
(define (list-copy list)
  (if (null? list)
      '()
      (cons (car list)
            (list-copy (cdr list)))))


            	
;;-------------------- Here is the initial global environment --------

(define *global-env*
  (list 
 	  (list 'car (list 'primitive-function car))
 	  (list 'set-cdr! (list 'primitive-function set-cdr!))
 	  (list 'set-car! (list 'primitive-function set-car!))
		(list 'cdr (list 'primitive-function cdr))
		(list 'cons (list 'primitive-function cons))
		(list 'list (list 'primitive-function list))	
		(list 'error (list 'primitive-function error))	
		(list '+ (list 'primitive-function +))	
		(list '- (list 'primitive-function -))
		(list '* (list 'primitive-function *))
		(list '= (list 'primitive-function =))
		(list '< (list 'primitive-function <))
		(list '> (list 'primitive-function >))
		(list '<= (list 'primitive-function  <=))
		(list '>= (list 'primitive-function >=))
		(list 'eq? (list 'primitive-function eq?))		
		(list 'symbol? (list 'primitive-function symbol?))
		(list 'equal? (list 'primitive-function equal?))
		(list 'not (list 'primitive-function not))
		(list 'pair? (list 'primitive-function pair?))
		(list 'null? (list 'primitive-function null?))
		(list 'read (list 'primitive-function read))
		(list 'display (list 'primitive-function display))
		(list 'newline (list 'primitive-function newline))
		(list 'assoc (list 'primitive-function assoc))
		(list 'open-input-file (list 'primitive-function open-input-file))
		(list 'close-input-port (list 'primitive-function close-input-port))
		(list 'eof-object? (list 'primitive-function eof-object?))
		(list 'load (list 'primitive-function my-load))  
		(list 'apply (list 'primitive-function my-apply))
	)
)
