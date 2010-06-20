;busca en el ambiente env el elemento elem
;ambiente: ((var val) (var2 val2))
(defun env_search (env elem)
	(if (or (null env) (null elem))
		nil
		(if (eq (caar env) elem)
			(cadar env)
			(env_search (cdr env) elem)
		)
	)
)

;evalua una expresion quote
;(quote expresion)
(defun exec_quote (code)
	(cadr code)
)

;evalua una expresion que es un atomo
;atomo
(defun exec_atom (the_atom env)
	(if (numberp the_atom) 
		the_atom
		(if (eq the_atom 't)
			t
			(env_search env the_atom)
		)
	)
)

;evalua una expresion or
;(or expr1 expr2)
(defun exec_or (code env)
	(if (exec (cadr code) env)
		T
		(exec (caddr code) env)
	)
)

;evalua una expresion and
;(and expr1 expr2)
(defun exec_and (code env)
	(if (exec (cadr code) env)
		(exec (caddr code) env)
		nil
	)
)

;evalua una expresion if
;(if expresion true-code false-code)
(defun exec_if (code env)
	(if (exec (cadr code) env)
		(exec (caddr code) env)
		(exec (cadddr code) env)
	)
)

;evalula una expresion cond
;(cond (expr code) (expr code))
(defun exec_cond (code env)
	(exec_cond_list (cdr code) env)
)

;evalua una lista de expresion del cond con el sig formato
;((expr code) (expr code) )
(defun exec_cond_list (code env)
	(if (exec (caar code) env)
		(exec (cadar code) env)
		(exec_cond_list (cdr code) env)
	)
)

;procesamos funciones de lisp
;(fun params)
(defun exec_fun (code env)
	(cond 
		((eq (car code) 'list) (cdr code))
		((eq (car code) 'car) (my_car code))
		((eq (car code) 'cdr) (my_cdr code))
		((eq (car code) 'caar) (car (my_car code)))
		((eq (car code) 'cdar) (cdr (my_car code)))
		(t nil)
	)
)

;hace car del codigo
;(fun (val1 val2))
;retorna val1
(defun my_car (code)
	(caadr code)
)

;hace cdr del codigo
;(fun (val1 val2))
;retorna (val2)
(defun my_cdr (code)
	(cdadr code)
)

;evalue la lista de argumentos de una funcion
;(fun param1 param2 ... paramn)
(defun eval_args (code env)
	(cons (car code) (eval_args_list (cdr code) env))
)

;evalue la lista de argumentos de una funcion
;(param1 param2 ... paramn)
(defun eval_args_list (code env)
	(if (null code)
		nil
		(cons (exec (car code) env) (eval_args_list (cdr code) env))
	)
)

;evalua una expresion lisp
;ver ejemplos mas abajo
(defun exec (code &optional (env nil))
	(if (null code) nil
		(cond
			;procesa atomos (numeros y variables de ambiente)
			((atom code) (exec_atom code env))
			;procesa el quote
			((eq (car code) 'quote) (exec_quote code))
			;procesamos el or
			((eq (car code) 'or) (exec_or code env))
			;procesamos el and
			((eq (car code) 'and) (exec_and code env))
			;procesamos el if
			((eq (car code) 'if) (exec_if code env))
			;procesamos cond
			((eq (car code) 'cond) (exec_cond code env))
			;procesamos demas funciones
			(t (exec_fun (eval_args code env) env))
		)
	)
)

;testing function
;=============================
(defun test (name got expected)
	(if (equal expected got)
		t
		(progn (print '==errr==) (print name) (print 'expected) (print expected) (print 'got) (print got))
	)
)
;=============================

;numeros
(test 'numero1 (exec '1) '1)
(test 'numero2 (exec '2) '2)

;true false
(test 'tf1 (exec nil) nil)
(test 'tf2 (exec 'nil) nil)
(test 'tf3 (exec 't) t)

;variables de ambiente
(test 'amb1 (exec 'A '((A 2)) ) '2)
(test 'amb2 (exec 'B '((A 2) (B 10)) ) '10)

;quote
(test 'quote1 (exec '(quote A) ) 'A)
(test 'quote2 (exec '(quote 1) ) '1)
(test 'quote3 (exec '(quote (car a)) ) '(car a))

;or
(test 'or1 (exec '(or t t) ) 't)
(test 'or2 (exec '(or t nil) ) 't)
(test 'or3 (exec '(or nil nil) ) 'nil)
(test 'or4 (exec '(or nil t) ) 't)

;and
(test 'and1 (exec '(and nil nil) ) 'nil)
(test 'and2 (exec '(and t nil) ) 'nil)
(test 'and3 (exec '(and nil t) ) 'nil)
(test 'and4 (exec '(and t t) ) 't)

;and y or
(test 'andor1 (exec '(and (or t nil) t) ) 't)
(test 'andor2 (exec '(and (or t nil) (or nil nil)) ) 'nil)
(test 'andor3 (exec '(or (or t nil) (or nil nil )) ) 't)

;if
(test 'if1 (exec '(if t 1 2)) '1)
(test 'if2 (exec '(if t 1 2)) '1)
(test 'if3 (exec '(if (or t nil) 1 2)) '1)

;cond
(test 'cond1 (exec '(cond (t 2) )) '2)
(test 'cond2 (exec '(cond (nil 5) (t 2) )) '2)
(test 'cond3 (exec '(cond (and (nil nil) 5) (t 2) )) '2)
(test 'cond4 (exec '(cond (and (nil nil) 5) (nil 99) (t 2) )) '2)

;list
(test 'list1 (exec '(list 2 3 4)) '(2 3 4))
(test 'list2 (exec '(list 2 3 4 5)) '(2 3 4 5))
(test 'list3 (exec '(list t)) '(t))

;list con proceso
(test 'listproc1 (exec '(list (or t t))) '(t))
(test 'listproc2 (exec '(list (or t t) (and t nil))) '(t nil))
(test 'listproc3 (exec '(list (quote (2 3 4)))) '((2 3 4)))

;car
(test 'car1 (exec '(car (quote (2 3)))) '2)
(test 'car2 (exec '(car (quote (4 2 3)))) '4)

;cdr
(test 'cdr1 (exec '(cdr (quote (4 2 3)))) '(2 3))

;caar
(test 'caar1 (exec '(caar (quote ((4 2 3))))) '4)

;cdar
(test 'cdar1 (exec '(cdar (quote ((4 2 3))))) '(2 3))

;car + ambiente
(test 'car-amb1 (exec '(car (list a 2 3)) '((a 100)) ) '100)

;cdr + ambiente
(test 'cdr-amb1 (exec '(cdr (list a b c)) '((a 100) (b 99) (c 98)) ) '(99 98))
