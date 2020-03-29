(define-syntax mylet
  (syntax-rules ()
    [(_ ((x e) ...) b1 b2 ...)
     ((lambda (x ...) b1 b2 ...) e ...)]))

(define-syntax myand
  (syntax-rules ()
    [(_) #t]
    [(_ e) e]
    [(_ e1 e2 e3 ...)
     (if e1 (myand e2 e3 ...) #f)]))

(define-syntax myor
  (syntax-rules ()
    [(_) #f]
    [(_ e) e]
    [(_ e1 e2 e3 ...)
     (let ([t e1])
       (if t t (myor e2 e3 ...)))]))

(define-syntax mylet*
  (syntax-rules ()
    [(_ () e1 e2 ...)
     (let () e1 e2 ...)]
    [(_ ((x1 v1) (x2 v2) ...) e1 e2 ...)
     (let ((x1 v1))
       (let* ((x2 v2) ...) e1 e2 ...))]))

(define-syntax mywhen
  (syntax-rules ()
    [(_ e0 e1 e2 ...)
     (if e0 (begin e1 e2 ...))]))

(define-syntax myunless
  (syntax-rules ()
    [(_ e0 e1 e2 ...)
     (if (not e0) (begin e1 e2 ...))]))

(let ([f (lambda (x) (+ x 1))])
  (let-syntax ([f (syntax-rules ()
		    [(_ x) x])]
	       [g (syntax-rules ()
		    [(_ x) (f x)])])
    (list (f 1) (g 1))))
(let ([f (lambda (x) (+ x 1))])
  (letrec-syntax ([f (syntax-rules ()
		       [(_ x) x])]
		  [g (syntax-rules ()
		       [(_ x) (f x)])])
    (list (f 1) (g 1))))

(define-syntax mycond1
  (syntax-rules (else)
    [(_ (else e1 e2 ...)) (begin e1 e2 ...)]
    [(_ (e0 e1 e2 ...)) (if e0 (begin e1 e2 ...))]
    [(_ (e0 e1 e2 ...) c1 c2 ...)
     (if e0 (begin e1 e2 ...) (cond c1 c2 ...))]))

(let ()
  (define-syntax a (identifier-syntax car))
  (list (a '(1 2 3)) a))

(let ([ls (list 0)])
  (define-syntax a
    (identifier-syntax
     [id (car ls)]
     [(set! id e) (set-car! ls e)]))
  (let ([before a])
    (set! a 1)
    (list before a ls)))

(define-syntax myor1
  (lambda (x)
    (syntax-case x ()
      [(_) #'#f]
      [(_ e) #'e]
      [(_ e1 e2 e3 ...)
       #'(let ([t e1]) (if t t (myor1 e2 e3 ...)))])))

(define-syntax mysyntax-rules
  (lambda (x)
    (syntax-case x ()
      [(_ (i ...) ((keyword . pattern) template) ...)
       #'(lambda (x)
	   (syntax-case x (i ...)
	     [(_ . pattern) #'template] ...))])))

(define-syntax mylet1
  (lambda (x)
    (define ids?
      (lambda (ls)
	(or (null? ls)
	    (and (identifier? (car ls))
		 (ids? (cdr ls))))))
    (syntax-case x ()
      [(_ ((i e) ...) b1 b2 ...)
       (ids? #'(i ...))
       #'((lambda (i ...) b1 b2 ...) e ...)])))

(let ([p (cons 0 #f)])
  (define-syntax pcar
    (lambda (x)
      (syntax-case x ()
	[_ (identifier? x) #'(car p)]
	[(_ e) #'(set-car! p e)])))
    (let ([a pcar])
      (pcar 1)
      (list a pcar)))

(define-syntax mycond2
  (lambda (x)
    (syntax-case x ()
      [(_ (e0 e1 e2 ...))
       (and (identifier? #'e0) (free-identifier=? #'e0 #'else))
       #'(begin e1 e2 ...)]
      [(_ (e0 e1 e2 ...)) #'(if e0 (begin e1 e2 ...))]
      [(_ (e0 e1 e2 ...) c1 c2 ...)
       #'(if e0 (begin e1 e2 ...) (cond c1 c2 ...))])))

(define-syntax mylet2
  (lambda (x)
    (define ids?
      (lambda (ls)
	(or (null? ls)
	    (and (identifier? (car ls)) (ids? (cdr ls))))))
    (define unique-ids?
      (lambda (ls)
	(or (null? ls)
	    (and (not (memp
		       (lambda (x) (bound-identifier=? x (car ls)))
		       (cdr ls)))
		 (unique-ids? (cdr ls))))))
    (syntax-case x ()
      [(_ ((i e) ...) b1 b2 ...)
       (and (ids? #'(i ...)) (unique-ids? #'(i ...)))
       #'((lambda (i ...) b1 b2 ...) e ...)])))

(define-syntax mywith-syntax
  (lambda (x)
    (syntax-case x ()
      [(_ ((p e) ...) b1 b2 ...)
       #'(syntax-case (list e ...) ()
	   [(p ...) (let () b1 b2 ...)])])))

(define-syntax mycond3
  (lambda (x)
    (syntax-case x ()
      [(_ c1 c2 ...)
       (let f ([c1 #'c1] [cmore #'(c2 ...)])
	 (if (null? cmore)
	     (syntax-case c1 (else =>)
	       [(else e1 e2 ...) #'(begin e1 e2 ...)]
	       [(e0) #'(let ([t e0]) (if t t))]
	       [(e0 => e1) #'(let ([t e0]) (if t (e1 t)))]
	       [(e0 e1 e2 ...) #'(if e0 (begin e1 e2 ...))])
	     (with-syntax
	      ([rest (f (car cmore) (cdr cmore))])
	      (syntax-case c1 (=>)
		[(e0) #'(let ([t e0]) (if t t rest))]
		[(e0 => e1) #'(let ([t e0]) (if t (e1 t) rest))]
		[(e0 e1 e2 ...)
		 #'(if e0 (begin e1 e2 ...) rest)]))))])))

(define-syntax mycase
  (lambda (x)
    (syntax-case x ()
      [(_ e c1 c2 ...)
       #`(let ([t e])
	   #,(let f ([c1 #'c1] [cmore #'(c2 ...)])
	       (if (null? cmore)
		   (syntax-case c1 (else)
		     [(else e1 e2 ...) #'(begin e1 e2 ...)]
		     [((k ...) e1 e2 ...)
		      #'(if (memv t '(k ...)) (begin e1 e2 ...))])
		   (syntax-case c1 ()
		     [((k ...) e1 e2 ...)
		      #`(if (memv t '(k ...))
			    (begin e1 e2 ...)
			    #,(f (car cmore) (cdr cmore)))]))))])))
		     
(let ([ls (list 0)])
  (define-syntax a
    (make-variable-transformer
     (lambda (x)
       (syntax-case x ()
	 [id (identifier? #'id) #'(car ls)]
	 [(set! _ e) #'(set-car! ls e)]
	 [(_ e ...) #'((car ls) e ...)]))))
  (let ([before a])
    (set! a 1)
    (list before a ls)))

(define-syntax myidentifier-syntax
  (lambda (x)
    (syntax-case x (set!)
      [(_ e)
       #'(lambda (x)
	   (syntax-case x ()
	     [id (identifier? #'id) #'e]))]
      [(_ (id exp1) ((set! var val) exp2))
       (and (identifier? #'id) (identifier? #'var))
       #'(make-variable-transformer
	  (lambda (x)
	    (syntax-case x (set!)
	      [(set! var val) #'exp2]
	      [(id x (... ...)) #'(exp1 x (... ...))]
	      [id (identifier? #'id) #'exp1])))])))

(define-syntax loop
  (lambda (x)
    (syntax-case x ()
      [(k e ...)
       (with-syntax
	([bk (datum->syntax #'k 'bk)])
	#'(call/cc
	   (lambda (bk)
	     (let f () e ... (f)))))])))

(define-syntax myinclude
  (lambda (x)
    (define read-file
      (lambda (fn k)
	(let ([p (open-input-file fn)])
	  (let f ([x (read p)])
	    (if (eof-object? x)
		(begin (close-port p) '())
		(cons (datum->syntax k x) (f (read p))))))))
    (syntax-case x ()
      [(k filename)
       (let ([fn (syntax->datum #'filename)])
	 (with-syntax ([(expr ...) (read-file fn #'k)])
		      #'(begin expr ...)))])))

(define-syntax myletrec
  (lambda (x)
    (syntax-case x ()
      [(_ ((i e) ...) b1 b2 ...)
       (with-syntax ([(t ...) (generate-temporaries #'(i ...))])
		    #'(let ([i #f] ...)
			(let ([t e] ...)
			  (set! i t)
			  ...
			  (let () b1 b2 ...))))])))

(define-syntax lvhelp
  (syntax-rules ()
    [(_ (x1 .fmls) (x ...) (t ...) e m b)
     (lvhelp fmls (x ... x1) (t ... tmp) e m b)]
    [(_ () (x ...) (t ...) e m b)
     (call-with-values
	 (lambda () e)
       (lambda (t ...)
	 (mylet-values m (let ([x t] ...) . b))))]
    [(_ xr (x ...) (t ...) e m b)
     (call-with-values
	 (lambda () e)
       (lambda (t ... . tmpr)
	 (mylet-values m (let ([x t] ... [xr tmpr]) . b))))]))
;;little mistake
(define-syntax mylet-values
  (syntax-rules ()
    [(_ () f1 f2 ...) (let () f1 f2 ...)]
    [(_ ((fmls1 expr1) (fmls2 expr2) ...) f1 f2 ...)
     (lvhelp fmls1 () () expr1 ((fmls2 expr2 ...)) (f1 f2 ...))]))

(define-syntax myrec
  (syntax-rules ()
    [(_ x e) (letrec ([x e]) x)]))

(map (rec sum
	  (lambda (x)
	    (if (= x 0)
		0
		(+ x (sum (- x 1))))))
     '(0 1 2 3 4 5))

(define-syntax mylet3
  (syntax-rules ()
    [(_ ((x e) ...) b1 b2 ...)
     ((lambda (x ...) b1 b2 ...) e ...)]
    [(_ f ((x e) ...) b1 b2 ...)
     ((rec f (lambda (x ...) b1 b2 ...)) e ...)]))

(define-syntax mylet4
  (syntax-rules ()
    [(_ ((x e) ...) b1 b2 ...)
     ((lambda (x ...) b1 b2 ...) e ...)]
    [(_ f ((x e) ...) b1 b2 ...)
     ((letrec ([f (lambda (x ...) b1 b2 ...)]) f) e ...)]))

(define-syntax mylet5
  (lambda (x)
    (syntax-case x ()
      [(_ ((x e) ...) b1 b2 ...)
       #'((lambda (x ...) b1 b2 ...) e ...)]
      [(_ f ((x e) ...) b1 b2 ...)
       (identifier? #'f)
       #'((rec f (lambda (x ...) b1 b2 ...)) e ...)])))

(define-syntax mylet6
  (lambda (x)
    (syntax-case x ()
      [(_ f ((x e) ...) b1 b2 ...)
       (identifier? #'f)
       #'((rec f (lambda (x ...) b1 b2 ...)) e ...)]
      [(_ ((x e) ...) b1 b2 ...)
       #'((lambda (x ...) b1 b2 ...) e ...)])))

(define-syntax mydo
  (lambda (x)
    (syntax-case x ()
      [(_ (binding ...) (test res ...) expr ...)
       (with-syntax
	([((var val update) ...)
	  (map (lambda (b)
		 (syntax-case b ()
		   [(var val) #'(var val var)]
		   [(var val update) #'(var val update)]))
	       #'(binding ...))])
	#'(let doloop ([var val] ...)
	    (if test
		(begin (if #f #f) #;look-odd res ...)
		(begin expr ... (doloop update ...)))))])))

(define-syntax be-like-begin
  (syntax-rules ()
    [(_ name)
     (define-syntax name
       (syntax-rules ()
	 [(_ e0 e1 (... ...))
	  (begin e0 e1 (... ...))]))]))

(let-syntax ([if (lambda (x)
		   (syntax-case x ()
		     [(_ e1 e2 e3)
		      #'(if e1 e2 e3)]))])
  (if (< 1 5) 2 3))

(letrec-syntax ([if (lambda (x)
		      (syntax-case x ()
			[(_ e1 e2 e3)
			 #'(if e1 e2 e3)]))])
  (if (< 1 5) 2 3))

(let-syntax ([divide (lambda (x)
		       (let ([/ +])
		       (syntax-case x ()
			 [(_ e1 e2) 
			   #'(/ e1 e2)])))])
  (let ([/ *])(divide 2 1)))
;;error

(define-syntax define-integrate
  (syntax-rules (lambda)
    [(_ name (lambda formals form1 form2 ...))
     (begin
       (define xname (lambda formals form1 form2 ...))
       (define-syntax name
	 (lambda (x)
	   (syntax-case x ()
	     [_ (identifier? x) #'xname]
	     [(_ arg (... ...))
	      #'((lambda formals form1 form2 ...)
		 arg (... ...))]))))]))

(define-syntax defien-integrable
  (syntax-rules (lambda)
    [(_ name (lambda formals form1 form2 ...))
     (begin
       (define xname
	 (let-syntax ([name (identifier-syntax xname)])
	   (lambda formals form1 form2 ...)))
       (define-syntax name
	 (lambda (x)
	   (syntax-case x ()
	     [_ (identifier? x) #'xname]
	     [(_ arg (... ...))
	      #'((let-syntax ([name (identifier-syntax xname)])
		   (lambda formals form1 form2 ...))
		 arg (... ...))]))))]))

(let ([x 0])
  (define-syntax x++
    (identifier-syntax
     (let ([t x])
       (set! x (+ t 1)) t)))
  (let ([a x++]) (list a x)))

(define-syntax method
  (lambda (x)
    (syntax-case x ()
      [(k (ivar ...) formals b1 b2 ...)
       (with-syntax
	([(index ...)
	  (let f ([i 0] [ls #'(ivar ...)])
	    (if (null? ls) '()
		(cons i (f (+ i 1) (cdr ls)))))]
	 [self (datum->syntax #'k 'self)]
	 [set! (datum->syntax #'k 'set!)])
	#'(lambda (self . formals)
	    (let-syntax ([ivar (identifier-syntax
				(vector-ref self index))]
			 ...)
	      (let-syntax ([set!
			    (syntax-rules (ivar ...)
			      [(_ ivar e) (vector-set! self index e)]
			      ...
			      [(_ x e) (set! x e)])])
		b1 b2 ...))))])))

(define-syntax method
  (lambda (x)
    (syntax-case x ()
      [(k (ivar ...) formals b1 b2 ...)
       (with-syntax
	([(index ...)
	  (let f ([i 0] [ls #'(ivar ...)])
	    (if (null? ls) '()
		(cons i (f (+ i 1) (cdr ls)))))]
	 [self (datum->syntax #'k 'self)])
	#'(lambda (self . formals)
	    (let-syntax ([ivar (identifier-syntax
				[_ (vector-ref self index)]
				[(set! _ e)
				 (vector-set! self index e)])] ...)
	      b1 b2 ...)))])))

(define-syntax define-structure
  (lambda (x)
    (define gen-id
      (lambda (template-id . args)
	(datum->syntax
	 template-id
	 (string->symbol
	  (apply string-append
		 (map (lambda (x)
			(if (string? x) x
			    (symbol->string (syntax->datum x))))
		      args))))))
    (syntax-case x ()
      [(_ name field ...)
       (with-syntax
	([constructor (gen-id #'name "make-" #'name)]
	 [predicate (gen-id #'name #'name "?")]
	 [(access ...)
	  (map (lambda (x) (gen-id x #'name "-" x))
	       #'(field ...))]
	 [(assign ...)
	  (map (lambda (x) (gen-id x "set-" #'name "-" x "!"))
	       #'(field ...))]
	 [structure-length (+ (length #'(field ...)) 1)]
	 [(index ...)
	  (let f ([i 1] [ids #'(field ...)])
	    (if (null? ids) '()
		(cons i (f (+ i 1) (cdr ids)))))])
	#'(begin
	    (define constructor
	      (lambda (field ...)
		(vector 'name field ...)))
	    (define predicate
	      (lambda (x)
		(and (vector? x)
		     (= (vector-length x) structure-length)
		     (eq? (vector-ref x 0) 'name))))
	    (define access
	      (lambda (x)
		(vector-ref x index)))
	    ...
	    (define assign
	      (lambda (x update)
		(vector-set! x index update)))
	    ...))])))
