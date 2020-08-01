(define (atom? e)
  (and (not (pair? e)) (not (null? e))))

(define name-of cadr)
(define right-side-of
  (lambda (x)
    (cond
     ((null? (cddr x)) 0)
     (else (caddr x)))))
      
;;end
(define the-empty-table
  (lambda (name)
    (abort
     (cons 'no-answer (cons name '())))))
(define global-table)
(define lookup
  (lambda (table name)
    (table name)))
(define lookup-in-global-table
  (lambda (name)
    (lookup global-table name)))

(define extend
  (lambda (name1 value table)
    (lambda (name2)
      (cond
       ((eq? name2 name1) value)
       (else
	(table name2))))))
;;end
(define box
  (lambda (it)
    (lambda (sel)
      (sel it (lambda (new)
		(set! it new))))))
(define setbox
  (lambda (box new)
    (box (lambda (it set) (set new)))))
(define unbox
  (lambda (box)
    (box (lambda (it set) it))))
;;end
(define define?
  (lambda (e)
    (cond
     ((atom? e) #f)
     ((atom? (car e))
      (eq? (car e) 'define))
     (else #f))))

(define *define
  (lambda (e)
    (set! global-table
	  (extend (name-of e)
		  (box
		   (the-meaning
		    (right-side-of e)))
		  global-table))))
;;end
(define *set
  (lambda (e table)
    (setbox
     (lookup table (name-of e))
     (meaning (right-side-of e) table))))
;;end
(define *identifier
  (lambda (e table)
    (unbox (lookup table e))))
;;end
(define *quote
  (letrec ((text-of cadr))
    (lambda (x table)
      (text-of x))))
;;end
(define beglis
  (lambda (es table)
    (cond
     ((null? (cdr es))
      (meaning (car es) table))
     (else ((lambda (val)
	      (beglis (cdr es) table))
	    (meaning (car es) table))))))
(define box-all
  (lambda (vals)
    (cond
     ((null? vals) '())
     (else (cons (box (car vals))
		 (box-all (cdr vals)))))))
(define multi-extend
  (lambda (names value table)
    (cond
     ((null? names) table)
     (else
      (extend (car names) (car value)
	      (multi-extend (cdr names) (cdr value) table))))))


(define *lambda
  (letrec ((formals-of cadr)
	   (body-of cddr))
    (lambda (e table)
      (lambda (args)
	(beglis (body-of e)
		(multi-extend
		 (formals-of e)
		 (box-all args)
		 table))))))
;;end
(define evlis
  (lambda (args table)
    (cond
     ((null? args) '())
     (else
      ((lambda(val)
	 (cons val (evlis (cdr args) table)))
       (meaning (car args) table))))))

(define *application
  (letrec ((function-of car)
	   (argument-of cdr))
    (lambda (e table)
      ((meaning (function-of e) table)
       (evlis (argument-of e) table)))))
;;end
(define a-prim
  (lambda (p)
    (lambda (args-in-a-list)
      (p (car args-in-a-list)))))
(define b-prim
  (lambda (p)
    (lambda (args-in-a-list)
      (p (car args-in-a-list)
	 (cadr args-in-a-list)))))
;;end
(define *const
  (let ((icons (b-prim cons))
	(icar (a-prim car))
	(icdr (a-prim cdr))
	(inull? (a-prim null?))
	(ieq? (b-prim eq?))
	(iatom? (a-prim atom?))
	(izero? (a-prim zero?))
	(iadd1 (a-prim add1))
	(isub1 (a-prim sub1))
	(inumber? (a-prim number?)))
    (lambda (e table)
      (cond
       ((number? e) e)
       ((eq? e #t) #t)
       ((eq? e #f) #f)
       ((eq? e 'cons) icons)
       ((eq? e 'car) icar)
       ((eq? e 'cdr) icdr)
       ((eq? e 'eq)  ieq?)
       ((eq? e 'atom?) iatom?)
       ((eq? e 'null?) inull?)
       ((eq? e 'zero?) izero?)
       ((eq? e 'add1) iadd1)
       ((eq? e 'sub1) isub1)
       ((eq? e 'number?) inumber?)))))

;;end
(define evcon
  (letrec ((question-of car)
	   (answer-of cadr)
	   (else? (lambda (x)
		    (cond
		     ((atom? x) (eq? x 'else))
		     (else #f)))))
    (lambda (lines table)
      (cond
       ((else? (question-of (car lines)))
	(meaning (answer-of (car lines)) table))
       ((meaning (question-of (car lines)) table)
	(meaning (answer-of (car lines)) table))
       (else (evcon (cdr lines) table))))))

(define *cond
  (letrec ((cond-lines-of cdr))
    (lambda (e table)
      (evcon (cond-lines-of e) table))))
;;end
(define *letcc
  (letrec ((ccbody-of cddr))
    (lambda (e table)
      (call/cc (lambda (skip)
		 (beglis (ccbody-of e)
			 (extend
			  (name-of e)
			  (box (a-prim skip))
			  table)))))))
;;end
(define atom-to-action
  (lambda (e)
    (cond
     ((number? e) *const)
     ((eq? e #t) *const)
     ((eq? e #f) *const)
     ((eq? e 'cons) *const)
     ((eq? e 'car) *const)
     ((eq? e 'cdr) *const)
     ((eq? e 'null?) *const)
     ((eq? e 'eq?) *const)
     ((eq? e 'atom?) *const)
     ((eq? e 'zero?) *const)
     ((eq? e 'add1) *const)
     ((eq? e 'sub1) *const)
     ((eq? e 'number?) *const)
     (else *identifier))))

(define list-to-action
  (lambda (e)
    (cond
     ((atom? (car e))
      (cond
       ((eq? (car e) 'quote) *quote)
       ((eq? (car e) 'lambda) *lambda)
       ((eq? (car e) 'letcc) *letcc)
       ((eq? (car e) 'set!) *set)
       ((eq? (car e) 'cond) *cond)
       (else *application)))
     (else *application))))

(define expression-to-action
  (lambda (e)
    (cond
     ((atom? e) (atom-to-action e))
     (else (list-to-action e)))))
(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))
(define the-meaning
  (lambda (e)
    (meaning e lookup-in-global-table)))

(define abort #f)
(define value
  (lambda (e)
    (call/cc (lambda (the-end)
	       (set! abort the-end)
	       (cond
		((define? e) (*define e))
		(else (the-meaning e)))))))

(value '(define value
	  (lambda (e)
	    (letcc the-end
		   (set! abort the-end)
		   (cond
		    ((define? e) (*define e))
		    (else
		     (the-meaning e)))))))
