(define-syntax ao
  (syntax-rules()
    [(_ p1 p2 . p3)
     (list p1 p2 'p3)]))

(define-syntax cao
  (syntax-rules()
    [(_ p1 p2 pk ... p3 p4)
     (list p1 p2 p3 p4 pk ...)]))

(define-syntax tony
  (syntax-rules()
    [(_ pk ... p1 p2)
     (list p1 p2 pk ...)]))

(define-syntax tom
  (syntax-rules()
    [(_ p1 p2 p ... p3 p4 . pn)
     (list p1 p2 p3 p4 p ... 'pn)]))

(define-syntax julia
  (syntax-rules()
    [(_ p1 #(p2 p3))
     (list p1 p2)]))

(define-syntax last
  (syntax-rules(else)
    [(_ else) 1]))

(define-syntax fst
  (lambda (x)
    (syntax-case x ()
      [(_ e) #'e])))

(define-syntax sr
  (lambda (x)
    (syntax-case x ()
      [(_ (i ...) ((keyword . patterm) template) ...)
       #'(lambda (x)
	   (syntax-case x (i ...)
	     [(_ . pattern) #'template] ...))])))

(define lst '(1 2))
(define-syntax pcar
  (lambda (x)
    (syntax-case x ()
      [_ (car lst)])))


(expand '(define Me (syntax-rules() [(_) 1])))


(define-syntax mao
  (syntax-rules ()
    [(_ a) ((... ...) a)]))

(define-syntax elcp
  (syntax-rules ()
    [(_ a) '(... ...)]))

(time (make-bytevector 10))

(define-syntax cao
  (syntax-rules ()
    [(_ a) a]))

(define-syntax ca
  (lambda (x)
    (syntax-case x ()
      [(_ a) #'a])))

(define-syntax cb
  (lambda (x)
    (syntax-case x ()
      [(_ a) #''(a)])))


(define-syntax cb
  (lambda (x)
    (syntax-case x ()
      [(_ a) #'a])))

(define-syntax mmp
  (lambda (x)
    (syntax-case x ()
      [(_ a b c)
       #'(a b c)])))

(define-syntax syntax-rule
  (lambda (x)
    (syntax-case x ()
      [(_ (i ...) ((keyword . pattern) template) ...)
       #'(lambda (x)
	   (syntax-case x (i ...)
	     [(_ . pattern) #'template] ...))])))

(define-syntax cao
  (syntax-rule
   ()
   [(_ a) (+ a 1)]))
   
(define-syntax nima
  (lambda (x)
    (syntax-case x ()
      [_ (identifier? x) #'car])))

(define-syntax cnd
  (lambda (x)
    (syntax-case x ()
      [(_ c1 c2 ...)
       (let ([c1 #'c1] [cmore #'(c2 ...)])
	 (cons c1 cmore))])))

(set! a 1)
(cond
 ((= 1 a) 2)
 (else 1))

(define-syntax cmd
  (lambda (x)
    (display x)
    (syntax-case x ()
      [(_ c1 c2 )
       #'(c1 c2 )])))

(define a #f)
(syntax-case
    (begin (set! a '(1 2 3)) #`#,a) ()
  [(c d e)
   (display a)
   #'(c d e)])

(define-syntax cn
  (lambda (x)
    (syntax-case x ()
      [(_ a b c) #'(list a b c)])))

(define-syntax com
  (lambda (x)
    (let ((n (cdr (syntax->datum x))))
      (display n)
      (syntax-case n ()
	[(a b c) #'(let ((ls #'(a b c)))
		     ls)]))))
	
(syntax-case #'(caonima 1 2 3) () [(caonima a b c)  #'(a b c)])

(syntax-case '(1 2 3) () [(a b c)  #'(a b c)])

((lambda (x) (syntax-case x () [(c1 c2 ...) #'(c1 c2 ...)])) (syntax 1))

(define-syntax com
  (lambda (x)
    (display x)
    (let ((n (cdr (syntax->datum x))))
      (syntax-case n ()
	[(a b c) #'(a b c) 1]))))

(define-syntax com
  (lambda (x)
    (let ((n (cdr (syntax->datum x))))
      (syntax-case n ()
	[(a b c) #'#'(a b c)]))))

(eval '(syntax-case '(1 2 3) () [(a b c)  #'(a b c)]))
(eval '(syntax-case '(1 2 3) () [(a b c)  (syntax (list a b c))]))

((lambda (x)
   (syntax-case x ()
     [(a b c) (syntax->datum #'(a b c))]))
 (syntax (1 2 3)))

(define-syntax wocao
  (lambda (x)
    (syntax-case x ()
      [(_ a b c)
       #'(lambda (x)
	   (cond
	    ((= x 1) a)
	    ((= x 2) b)
	    ((= x 3) c)))])))

(expand '(wocao 1 2 3))
(expand ' ((lambda (x)
	     (syntax-case x ()
	       [(_ a b c)
		#'(lambda (x)
		    (cond
		     ((= x 1) a)
		     ((= x 2) b)
		     ((= x 3) c)))]))
	   1))

(expand '(lambda (x)
  (syntax-case x ()
    [(_ a b)
     #'(let (c #'(a b))
	 c)])))

(define-syntax eqqq
  (lambda (a)
    (syntax-case a ()
      [(_ x y) #'(bound-identifier=? #'x #'y)])))

((lambda (x)
   (syntax-case x ()
     [(a b c) #'(let ((ls #'(a b c)))
		  ls)]))
 '(1 2 2))

(define-syntax with-syn
  (lambda (x)
    (syntax-case x ()
      [(_ ((p e) ...) b1 b2 ...)
       #'(syntax-case (list e ...) ()
	   [(p ...) (let () b1 b2 ...)])])))

(expand '(lambda (x)
	   (syntax-case x ()
	     [(_ ((p e) ...) b1 b2 ...)
	      #'(syntax-case (list e ...) ()
		  [(p ...) (let () b1 b2 ...)])])))

((lambda (x)
   (syntax-case x ()
     [(((p e) ...) b1 b2 ...)
      #'(syntax-case (list e ...) ()
	  [(p ...) (let () b1 b2 ...)])]))
 #'(((a 1)) 1))

(define-syntax condition
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
	     (with-syntax ([rest (f (car cmore) (cdr cmore))])
			  (syntax-case c1 (=>)
			    [(e0) #'(let ([t e0]) (if t t rest))]
			    [(e0 => e1) #'(let ([t e0]) (if t (e1 t) rest))]
			    [(e0 e1 e2 ...)
			     #'(if e0 (begin e1 e2 ...) rest)]))))])))

(define-syntax cases
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

(define-syntax identifier-syn
  (lambda (x)
    (syntax-case x (set!)
      [(_ e)
       #'(lambda (x)
	   (syntax-case x ()
	     [id (identifier? #'id) #'e]
	     [(_ x (... ...)) #'(e x (... ...))]))]
      [(_ (id exp1) ((set! var val) exp2))
       (and (identifier? #'id ) (identifier? #'var))
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
       (with-syntax ([break (datum->syntax #'k 'break)])
		    #'(call/cc
		       (lambda (break)
			 (let f () e ... (f)))))])))

(expand '(with-syntax ([break (datum->syntax #'k 'break)])
		      #'(call/cc
			 (lambda (break)
			   1))))

(define-syntax loop2
  (lambda (x)
    (syntax-case x ()
      [(_ k e ...)
       #`(syntax-case (list (datum->syntax #'k 'bake)) ()
	   [(bake)
	    (let ()
	      (call/cc
	       (lambda (bake)
		 (let f () e ... (f)))))])])))
			     
(let ([i 0])
  (loop (if (= i 9) (break))
	(display i) (newline)
	(set! i (+ i 1))))

(define-syntax included
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

(define-syntax letrec
  (lambda (x)
    (syntax-case x ()
      [(_ ((i e) ...) b1 b2 ...)
       (with-syntax ([(t ...) (generate-temporaries #'(i ...))])
		    #'(let ([i #f] ...)
			(let ([t e] ...)
			  (set! i t)
			  ...
			  (let () b1 b2 ...))))])))

(define-syntax let-valuess
  (syntax-rules ()
    [(_ () f1 f2 ...) (let () f1 f2 ...)]
    [(_ ((fmls1 expr1) (fmls2 expr2) ...) f1 f2 ...)
     (lvhelp fmls1 () () expr1 ((fmls2 expr2) ...) (f1 f2 ...))]))

(define-syntax lvhelp
  (syntax-rules ()
    [(_ (x1 . fmls) (x ...) (t ...) e m b)
     (lvhelp fmls (x ... x1) (t ... tmp) e m b)]
    [(_ () (x ...) (t ...) e m b)
     (call-with-values
	 (lambda () e)
       (lambda (t ...)
	 (let-valuess m (let ([x t] ...) . b))))]
    [(_ xr (x ...) (t ...) e m b)
     (call-with-values
	 (lambda () e)
       (lambda (t ... . tmpr)
	 (let-valuess m (let ([x t] ... [xr tmpr]) . b))))]))

(define-syntax rec
  (syntax-rules ()
    [(_ x e) (letrec ([x e]) x)]))

(define-syntax let
  (syntax-rules ()
    [(_ ((x e) ...) b1 b2 ...)
     ((lambda (x ...) b1 b2 ...) e ...)]
    [(_ f ((x e) ...) b1 b2 ...)
     ((rec f (lambda (x ...) b1 b2 ...)) e ...)]))

(define-syntax let
  (syntax-rules ()
    [(_ f ((x e) ...) b1 b2 ...)
     ((rec f (lambda (x ...) b1 b2 ...)) e ...)]
    [(_ ((x e) ...) b1 b2 ...)
     ((lambda (x ...) b1 b2 ...) e ...)]))

(define-syntax lett
  (lambda (x)
    (syntax-case x ()
      [(_ ((x e) ...) b1 b2 ...)
       #'((lambda (x ...) b1 b2 ...) e ...)]
      [(_ f ((x e) ...) b1 b2 ...)
       (identifier? #'f)
       #'((rec f (lambda (x ...) b1 b2 ...)) e ...)])))

(define-syntax does
  (lambda (x)
    (syntax-case x ()
      [(_ (binding ...) (test res ...) expr ...)
       (with-syntax ([((var val update) ...)
		      (map (lambda (b)
			     (syntax-case b ()
			       [(var val) #'(var val var)]
			       [(var val update) #'(var val update)]))
			   #'(binding ...))])
		    #'(let doloop ([var val] ...)
			(if test
			    (begin (if #f #f) res ...)
			    (begin expr ... (doloop update ...)))))])))

(define-syntax be-like-begin
  (syntax-rules ()
    [(_ name)
     (define-syntax name
       (syntax-rules ()
	 [(_ e0 e1 (... ...))
	  (begin e0 e1 (... ...))]))]))
(define-syntax bb-like-begin-wrong
  (syntax-rules ()
    [(_ name)
     (define-syntax name
       (syntax-rules ()
	 [(_ e0 e1 ...)
	  (begin e0 e1 ...)]))]))

(let-syntax ([divide (lambda (x)
		       (let ([/ +])
			 (syntax-case x ()
			   [(_ e1 e2) #'(/ e1 e2)])))])
  (let ([/ +])(divide 2 1)))

(define-syntax addd
  (lambda (x)
    (syntax-case x ()
      [(_ a b) #'(+ a b)])))
	   
(define-syntax define-integrable
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
		 arg
		 (... ...))]))))]))

(define-syntax define-integer
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

(define-syntax method
  (lambda (x)
    (syntax-case x ()
      [(k (ivar ...) formals b1 b2 ...)
       (with-syntax ([(index ...)
		      (let f ([i 0] [ls #'(ivar ...)])
			(if (null? ls)
			    '()
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
		    
(define-syntax with-math-defines
  (lambda (x)
    (syntax-case x ()
      ((with-math-defines expression)
       (with-syntax
	((expr (datum->syntax-object
                (syntax k)
		(begin
		  (display
                `(let ( (pi 3.14) (e 2.72))
                   ,(syntax->datum (syntax expression))))
		`(let ((pi 3.14) (e 2.72))
		   ,(syntax->datum #'expression))))))
	(display #'expr)
         #'expr)))))

(define-syntax with-math
  (lambda (x)
    (syntax-case x ()
      [(_ expression)
      (datum->syntax #'k
		     `(let ([pi 3])
			,(syntax->datum #'expression)))])))
(eval(
  (lambda (x)
    (display x)
  (syntax-case (datum->syntax (syntax a) x) ()
    [(a b c)
     (syntax->datum  (syntax (+ a b c)))]))
'(1 2 3)
))
 
(define-syntax now
  (lambda (x)
    (display x)
    (syntax-case x ()
      [(_ a b c) #'(+ a b c)])))

(define cao
  (lambda (x)
    (syntax-case x ()
      [(a) (syntax (+ a))]
      [(a b) (syntax (+ a b))])))
(define sao
  (lambda (x)
    (syntax->datum (cao (datum->syntax (syntax k) x)))))

(define-syntax pcar
  (lambda (x)
    (syntax-case x ()
      [_ (identifier? x) #'car]
      [(_ e) #'(car e)])))

(define-syntax cond1
  (lambda (x)
    (syntax-case x ()
      [(_ (e0 e1 e2 ...))
       (and (identifier? #'e0) (free-identifier=? #'e0 #'else))
       #'(begin e1 e2 ...)]
      [(_ (e0 e1 e2 ...))
       #'(if e0 (begin e1 e2 ...))]
      [(_ (e0 e1 e2 ...) c1 c2 ...)
       #'(if e0 (begin e1 e2 ...) (cond1 c1 c2 ...))])))

(define-syntax let
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

(define-syntax cond2
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
	     (with-syntax ([rest (f (car cmore) (cdr cmore))])
			  (syntax-case c1 (=>)
			    [(e0) #'(let ([t e0]) (if t t rest))]
			    [(e0 => e1) #'(let ([t e0]) (if t (e1 t) rest))]
			    [(e0 e1 e2 ...)
			     #'(if e0 (begin e1 e2 ...) rest)]))))])))


(define-syntax lit
  (lambda (x)
    (syntax-case x ()
      [(_ e0 e1 ...)
      (display (cons (datum->syntax #'k '+) (let f ((tst #'(e0 e1 ...)))
					  (cond
					   ((null? tst) '())
					   (else
					    (cons (car tst) (f  (cdr tst))))))))])))


(define-syntax with-math-defines
  (lambda (x)
    (syntax-case x ()
      [(_ expression)
        (with-syntax
	 ((expr (datum->syntax
                (syntax k)
                `(let ( (pi 3.14) (e 2.72))
                   ,(syntax->datum (syntax expression))))))
         (syntax expr))])))
		    
(define-syntax lit
  (lambda (x)
    (syntax-case x ()
      [(_ e0 e1 e2 ...)
       (let f ([ls #'(e0 e1 e2 ...)])
	 (cond
	  ((null? ls) '())
	  (else
	   (cons (car ls) (f (cdr ls))))))])))


(define-syntax loop
  (lambda (x)
    (syntax-case x ()
      [(k e ...)
       (with-syntax
	([bk (datum->syntax #'k 'bk)])
	#'(call/cc (lambda (bk)
		     (let f () e ... (f)))))])))
(let ([n 10] [ls '()])
  (loop (if (= n 0) (bk ls))
	(set! ls (cons 'a ls) )
	(set! n (- n 1))))


(define-syntax now
  (lambda (x)
    (syntax-case x ()
      [(k e ...)
       #`(call/cc (lambda (#,(datum->syntax #'k 'bk))
		    (let f () e ... (f))))])))
(let ([n 4])
  (now
   (if (= n 0) (bk 0))
   (display n)
   (set! n (- n 1))))

(define-syntax include
  (lambda (x)
    (define read-file
      (lambda (fn k)
	(let ([p (open-input-file fn)])
	  (let f ([x (read p)])
	    (if (eof-object? x)
		(begin (close-port p) '())
		(cons (datum->syntax )))))))))


(define flp
  (lambda (f)
    (lambda (x y)
      (f y x))))

(define-syntax flip
  (lambda (x)
    (syntax-case x ()
      [(_ exp)
       (with-syntax
	((syn (syntax->datum #'exp)))
	(with-syntax
	 ([fst (datum->syntax #'k (car #'syn))]
	  [sec (datum->syntax #'k (reverse (cadr #'syn)))]
	  [thr (cddr #'syn)])
	 (with-syntax
	  ((final (cons*  #'fst #'sec (let f ([ls #'thr])
					     (cond
					      ((null? ls) '())
					      (else
					       (let ((now (car ls)))
						 (cons (datum->syntax #'k now) (f (cdr ls))))))))))
	  #'final)))])))

(define-syntax flip
  (lambda (x)
    (syntax-case x ()
      [(_ (l (args ...) e0 e1 ...))
       #`(l #,(reverse #'(args ...)) e0 e1 ...)])))


(define-syntax explain
  (lambda (x)
    (define concate
      (lambda (env sym)
	(let* ([now (symbol->string sym)]
	       [len (string-length now)])
	  (let f ([i 0])
	    (cond
	     ((= i (- len 2))
	      (let ((ne (string-ref now i)))
		(if (char=? ne #\a)
		    (list (datum->syntax env 'car)
			  (datum->syntax env 'x))
		    (list (datum->syntax env 'cdr)
			  (datum->syntax env 'x)))))
	     ((char=? (string-ref now i) #\c)
	      (cons* (datum->syntax env 'lambda) (datum->syntax env '(x)) (list (f (+ i 1)))))
	     ((char=? (string-ref now i) #\a)
	      (cons (datum->syntax env 'car) (list (f (+ i 1)))))
	     ((char=? (string-ref now i) #\d)
	      (cons (datum->syntax env 'cdr) (list (f (+ i 1))))))))))
    (syntax-case x ()
      [(k cmp)
       (with-syntax
	([new (concate #'k (syntax->datum #'cmp))])
	#'new)])))
