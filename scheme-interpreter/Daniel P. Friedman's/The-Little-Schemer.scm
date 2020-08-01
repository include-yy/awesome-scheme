(define atom?
  (lambda (a)
    (and (not (pair? a)) (not (null? a)))))

(define (first lst)
  (car lst))
(define (second lst)
  (cadr lst))
(define (third lst)
  (caddr lst))
(define (build a b)
  (list a b))

(define extend-table cons)
(define new-entry build)

(define lookup-in-entry
  (letrec ((help (lambda (name names values entry-f)
		   (cond
		    ((null? names) (entry-f name))
		    ((eq? (car names) name)
		     (car values))
		    (else (help name (cdr names) (cdr values) entry-f))))))
    (lambda (name entry entry-f)
      (help name (first entry) (second entry) entry-f))))

(define lookup-in-table
  (lambda (name table table-f)
    (cond
     ((null? table) (table-f name))
     (else (lookup-in-entry
	    name (car table)
	    (lambda (name)
	      (lookup-in-table
	       name (cdr table) table-f)))))))

(define apply-primitive
  (letrec ((hatom? (lambda (x)
		    (cond
		     ((atom? x) #t)
		     ((null? x) #f)
		     ((eq? (car x) 'primitive) #t)
		     ((eq? (car x) 'non-primitive) #t)
		     (else #f)))))
    (lambda (name vals)
      (cond
       ((eq? name 'cons) (cons (first vals) (second vals)))
       ((eq? name 'car) (car (first vals)))
       ((eq? name 'cdr) (cdr (first vals)))
       ((eq? name 'null?) (null? (first vals)))
       ((eq? name 'eq?) (eq? (first vals) (second vals)))
       ((eq? name 'atom?) (hatom? (first vals)))
       ((eq? name 'zero?) (zero? (first vals)))
       ((eq? name 'add1) (add1 (first vals)))
       ((eq? name 'sub1) (sub1 (first vals)))
       ((eq? name 'number?) (number? (first vals)))
       ((eq? name '+) (+ (first vals) (second vals)))))))

(define apply-closure
  (letrec ((table-of first)
	   (formals-of second)
	   (body-of third))
  (lambda (closure vals)
    (meaning (body-of closure)
	     (extend-table
	      (new-entry
	       (formals-of closure)
	       vals)
	      (table-of closure))))))

(define applyme
  (letrec ((primitive?
	    (lambda (l)
	      (eq? (first l) 'primitive)))
	   (non-primitive?
	    (lambda (l)
	      (eq? (first l) 'non-primitive))))
    (lambda (fun vals)
      (cond
       ((primitive? fun)
	(apply-primitive (second fun) vals))
       ((non-primitive? fun)
	(apply-closure (second fun) vals))))))
;;end

(define *const
  (lambda (e table)
    (cond
     ((number? e) e)
     ((eq? e #t) #t)
     ((eq? e #f) #f)
     (else (build 'primitive e)))))

(define *quote
  (lambda (e table)
    (letrec ((text-of (lambda (e)
			(second e))))
      (text-of e))))

(define initial-table
  (lambda (name)
    (car '())))
(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

(define *lambda
  (lambda (e table)
    (build 'non-primitive (cons table (cdr e)))))

;;end
(define evcon
  (letrec ((question-of first)
	   (answer-of second)
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
(define evlis
  (lambda (args table)
    (cond
     ((null? args) '())
     (else
      (cons (meaning (car args) table)
	    (evlis (cdr args) table))))))
(define *application
  (letrec ((function-of car)
	   (arguments-of cdr))
    (lambda (e table)
      (applyme
       (meaning (function-of e) table)
       (evlis (arguments-of e) table)))))
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
     ((eq? e '+) *const)
     (else *identifier))))

(define list-to-action
  (lambda (e)
    (cond
     ((atom? (car e))
      (cond
       ((eq? (car e) 'quote) *quote)
       ((eq? (car e) 'lambda) *lambda)
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
