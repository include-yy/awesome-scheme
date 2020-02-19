(define middle
  (lambda (lat)
    (cond
     ((number? lat)
      lat)
     (else
      (apply (car (cdr lat))
	     (list(middle(car lat))
		  (middle(car(cdr(cdr lat))))))))))

(define one?
  (lambda (on lat)
    (cond
     ((null? lat)
      #f)
     ((equal? on (car lat))
      #t)
     (else
      (one? on (cdr lat))))))

(define set-in
  (lambda (lat)
    (not(one? (car lat) (cdr lat)))))

(define set?
  (lambda (lat)
    (cond
     ((null? (cdr lat))#t)
     (else
      (if (set-in lat)
	  (set? (cdr lat))
	  #f)))))

(define makeset
  (lambda(lat)
    (cond
     ((null? (cdr lat)) lat)
     ((one? (car lat)(cdr lat))
      (makeset (cdr lat)))
     (else
      (cons (car lat)(makeset (cdr lat)))))))

(define subset?
  (lambda (set1 set2)
    (cond
     ((null? set1) #t)
     ((one? (car set1) set2)
      (subset? (cdr set1) set2)))))

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2)
	 (subset? set2 set1))))

(define intersect?
  (lambda (set1 set2)
    (cond
     ((null? set1)#f)
     ((one? (car set1) set2)
      #t)
     (else
      (intersect? (cdr set1) set2)))))

(define intersect
  (lambda (set1 set2)
    (cond
     ((null? set1)'())
     (else
      (if(one? (car set1) set2)
	 (cons (car set1)(intersect (cdr set1) set2))
	 (intersect (cdr set1) set2))))))

(define union
  (lambda (set1 set2)
    (cond
     ((null? set1) set2)
     (else
      (if(not(one? (car set1) set2))
	 (cons (car set1)(union (cdr set1) set2))
	 (union (cdr set1) set2))))))

(define subsect
  (lambda (set1 set2)
    (cond
     ((null? set1)'())
     (else
      (if(not(one? (car set1) set2))
	(cons (car set1)(subsect (cdr set1)set2))
	(subsect (cdr set1)set2))))))

(define intersectall
  (lambda(l-set)
    (cond
     ((null?(cdr l-set))(car l-set))
     (else
      (intersect(car l-set)
		(intersectall(cdr l-set)))))))

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
       ((null? l)'())
       ((eq? (car l) old)
	(seq new old (cdr l)))
       (else
	(cons (car l)
	      ((insert-g seq) new old (cdr l))))))))

(define insertL
  (insert-g
   (lambda (new old l)
     (cons new (cons old l)))))

(define insertR
  (insert-g
   (lambda (new old l)
     (cons old (cons new l)))))

(define cao
  (lambda (x)
    (cao x)))

(define build
  (lambda (s1 s2)
    (cond
     (else
      (cons s1
	    (cons s2 '()))))))

(define first
  (lambda (p)
    (cond
     (else
      (car p)))))

(define second
  (lambda (p)
    (cond
     (else
      (car (cdr p))))))

(define shift
  (lambda (pair)
    (build (first (first pair))
	   (build (second (first pair))
		  (second pair)))))

(define align
  (lambda (pora)
    (cond
     ((atom? pora) pora)
     ((pair? (first pora))
      (align (shift pora)))
     (else
      (build (first pora)
	     (align (second pora)))))))

(define C
  (lambda (n)
    (cond
     ((= 1 n)'(1))
     (else
      (cond
       ((= 0 (remainder n 2))
	(cons n (C (/ n 2))))
       (else
	(cons n (C (add1 (* 3 n))))))))))

(define A
  (lambda (n m)
    (cond
     ((zero? n)(add1 m))
     ((zero? m)(A (sub1 n)1))
     (else (A (sub1 n)
	      (A n (sub1 m)))))))

(define multiremberco
  (lambda (a lat col)
    (cond
     ((null? lat)
      (col '() '()))
     ((eq? a (car lat))
      (multiremberco
       a (cdr lat)
       (lambda (newlat seen)
	 (col newlat
	      (cons (car lat) seen)))))
     (else
      (multiremberco
       a (cdr lat)
       (lambda (newlat seen)
	 (col (cons (car lat) newlat)
	      seen)))))))
(define col
  (lambda (a b)
    (cons a b)))

(define (plus a b)
  (let ([x a]
	[y b])
    (+ x y)))

(define pl
  (lambda (a b)
    ((lambda (x y)
       (+ x y)) a b)))

(define Y
  (lambda (le)
    ((lambda(f)(f f))
     (lambda(f)
       (le (lambda (x) ((f f) x)))))))

(define (div x y)
  (call/cc
   (lambda(return)
     (let
	 ((err (call/cc
		(lambda(returnErr)
		  (if (= y 0)
		      (returnErr "divi by zero")
		      (return (/ x y)))))))
       err))))


(define ni
  (lambda (a)
    (call/cc
     (lambda (x)
       (+ 1 (x 2))))))


(define (list-p li)
  (+ 1 (call/cc
   (lambda(exit)
     (let iter
	 ((rest li))
       (cond
	((null? rest)1)
	((zero? (car rest))(exit "zero"))
	(else
	 (* (car rest) (iter (cdr rest))))))))))


(define return #f)


(+ 1 (call/cc
      (lambda(cont)
	(set! return cont) 1)))


(let* ((yin
	((lambda (cc)
	   (write-char #\@)
	   cc) 
         (call/cc
	  (lambda (c) c))))        
       (yang
	((lambda (cc)
	   (write-char #\*)
	   cc) 
         (call/cc
	  (lambda (c) c)))))       
  (yin yin))


(define yin
  ((lambda(cc)
     (write-char #\@)
     cc)
   (call/cc
    (lambda(c)c))))


(define yang
  ((lambda(cc)
     (write-char #\^)
     cc)
   (call/cc
    (lambda(c)c))))


(define (yi cl)
  ((lambda(cc)
     (write-char #\@)
     cc)
   (call/cc
    (lambda (c)
      c)))
  cl)


(define (ya cl)
  ((lambda(cc)
     (write-char #\$)
     cc)
   (call/cc
    (lambda(c)
      c)))
  cl)


(define yi
  ((lambda(cc)
     (write-char #\@)
     cc)
   (call/cc
    (lambda (c)
      (c yi)))))
  

((lambda (a)
   (+ a 1))
 (call/cc
  (lambda(d)
    (d 1))))

(define yin
  ((lambda (cc)
     (write-char #\@)
     cc) 
   (call/cc
    (lambda (c) c))))

(define f
  (lambda(a)
    (+ 1 a)))


(define (generate-one-element-at-a-time lst)
  (define (control-state return)
    (for-each
     (lambda(element)
       (set! return (call/cc
		     (lambda(resume-here)
		       (set! control-state resume-here)
		       (return  element)))))
     lst)
    (return 'you-fell-off-the-end))

  (define (generator)
    (call/cc control-state))
  generator)


(define generate-digit
  (generate-one-element-at-a-time '(0 1 2 3)))


(define ret #f)
(define now
  (lambda(lat)
    (+ 1 (call/cc
	  (lambda(return)
	    (set! ret return)
	    (return 20))))))


(define (get-cc) (call/cc (lambda(c)c)))

(define (get-cc)
  (call/cc
   (lambda(c)
     c)))


(define get
  (lambda()
    (call/cc
     (lambda(x)
       x))))


(define lat
  (lambda(ls)
    (cond
     ((null? ls) #f)
     (else
      (cdr ls)))))


(define pri
  (lambda(a)
    (display a)
    5))


(define trial
  (lambda(a)
    (+ 1
       ((call/cc
	(lambda(h)
	  (+ 1 (h pri))))
	a))))


(let ([x (call/cc (lambda (k) k))])
  (x (lambda (ignore) "hi")))


(((call/cc (lambda(k) k)) (lambda(x) x)) "hi")


(define okn #f)


(define addop
  (lambda(a)
    (+ 1 (call/cc
	  (lambda(k)
	    (set! okn k)
	    (k a))))))


((
  (call/cc (lambda(k) k))
  (lambda(x) x)
  )
 "HEY")


(define abc
  (lambda(a)
    ((call/cc
      (lambda (k)k))
     (cond
      (( = a 0)(k 0))
      (else
       (+ 1 a))))))


(((call/cc (lambda(k)k))
  ((call/cc (lambda(o)o))
   (lambda(k) k)))
 "Ho")


(define retry #f)


(define factorial
  (lambda(x)
    (if (= x 0)
	(call/cc (lambda(k) (set! retry k) 1))
	(* x (factorial (- x 1))))))


(define member?
  (lambda(a lat)
    (cond
     ((null? lat)#f)
     (else
      (or (eq? a (car lat))
	  (member? a (cdr lat)))))))


(define multirember
  (lambda(a lat)
    ((Y (lambda(mr)
	  (lambda(lat)
	    (cond
	     ((null? lat)'())
	     ((eq? a (car lat))
	      (mr (cdr lat)))
	     (else
	      (cons (car lat)
		    (mr (cdr lat))))))))
     lat)))


(define Y
  (lambda(la)
    ((lambda(f)
       (f f))
     (lambda(fun)
       (la (lambda(x)((fun fun) x)))))))


(define length
  (Y (lambda(length)
       (lambda(l)
	 (cond
	  ((null? l)0)
	  (else
	   (add1 (length (cdr l)))))))))


(define murember
  (lambda(a lat)
    ((letrec
	 ((mr (lambda(lat)
		(cond
		 ((null? lat)'())
		 ((eq? a (car lat))
		  (mr (cdr lat)))
		 (else
		  (cons (car lat)
			(mr (cdr lat))))))))
       mr)
     lat)))


(define mul
  (letrec
      ((mul
	(lambda(a lat)
	  (cond
	   ((null? lat)'())
	   ((eq? (car lat)a)
	    (mul a (cdr lat)))
	   (else
	    (cons (car lat)
		  (mul a (cdr lat))))))))
    mul))


(define union
  (lambda(set1 set2)
    (letrec
	((U (lambda(set)
	      (cond
	       ((null? set)set2)
	       ((member?(car set)set2)
		(U (cdr set)))
	       (else
		(cons (car set)
		      (U (cdr set))))))))
      (U set1))))


(define union
  (lambda(set1 set2)
    (letrec
	((U (lambda(set)
	      (cond
	       ((null? set) set2)
	       ((M? (car set) set2)
		(U (cdr set)))
	       (else
		(cons (car set)
		      (U (cdr set)))))))
	 (M? (lambda(a lat)
	       (cond
		((null? lat)#f)
		((eq? (car lat) a)#t)
		(else
		 (M? a (cdr lat)))))))
      (U set1))))


(define interset
  (lambda(set1 set2)
    (cond
     ((null? set1)'())
     ((member? (car set1)set2)
      (cons (car set1)
	    (interset(cdr set1)set2)))
     (else
      (interset(cdr set1)set2)))))


(define interset2
  (lambda(set1 set2)
    (letrec
	((I (lambda(set)
	      (cond
	       ((null? set)'())
	       ((member? (car set) set2)
		(cons (car set)
		      (I (cdr set))))
	       (else
		(I (cdr set)))))))
      (I set1))))


(define intersetall
  (lambda(lset)
    (cond
     ((null? lset) '())
     ((null? (cdr lset)) (car lset))
     (else
      (interset(car lset)
	       (intersetall (cdr lset)))))))


(define intersetall
  (lambda(lset)
    (letrec
	((A (lambda(lset)
	      (cond
	       ((null? (cdr lset))
		(car lset))
	       (else
		(interset (car lset)
			  (A (cdr lset))))))))
      (cond
       ((null? lset)'())
       (else
	(A lset))))))


(define intersetall2
  (lambda(lset)
    (call/cc
     (lambda(ret)
       (letrec
	   ((A (lambda(lset)
		 (cond
		  ((null? (car lset))
		   (ret 'nil))
		  ((null? (cdr lset))
		   (car lset))
		  (else
		   (interset (car lset)
			     (A (cdr lset))))))))
	 (cond
	  ((null? lset)'())
	  (else
	   (A lset))))))))


(define intersetall3
  (lambda(lset)
    (call/cc
     (lambda(ret)
       (letrec
	   ((A (lambda(lset)
		 (cond
		  ((null? (car lset))
		   (ret 'nil))
		  ((null? (cdr lset))
		   (car lset))
		  (else (I (car lset)
			   (A (cdr lset)))))))
	    (I (lambda(s1 s2)
		 (letrec
		     ((J (lambda(s1)
			   (cond
			    ((null? s1) '())
			    ((member? (car s1 )s2)
			     (J (cdr s1)))
			    (else
			     (cons (car s1)
				   (J (cdr s1))))))))
		   (cond
		    ((null? s2) 'null)
		    (else
		     (J s1)))))))
	 (cond
	  ((null? lset) '())
	  (else (A lset))))))))


(define rember-beyond-first
  (lambda(a lat)
    (letrec
	((R (lambda(lat)
	      (cond
	       ((null? lat)'())
	       ((eq? (car lat) a)
		'())
	       (else
		(cons (car lat)
		      (R (cdr lat))))))))
      (R lat))))

(define rember-upto-last
  (lambda(a lat)
    (call/cc
     (lambda(skip)
       (letrec
	   ((R (lambda(lat)
		 (cond
		  ((null? lat)'())
		  ((eq? (car lat) a)
		   (skip (R (cdr lat))))
		  (else
		   (cons (car lat)
			 (R (cdr lat))))))))
	 (R lat))))))

(define cc
  (lambda(a b)
    a))

(define get
  (lambda(lat col)
    (cond
     ((null? lat)
      (col '() '()))
     (else
      (cond
       ((= (remainder (car lat) 2) 0)
	(get (cdr lat)
	     (lambda(las1 las2)
	       (col (cons (car lat) las1)
		    las2))))
       ((= (remainder (car lat) 2) 1)
	(get (cdr lat)
	     (lambda(las1 las2)
	       (col las1
		    (cons (car lat) las2))))))))))


(define get
  (lambda(lat)
    (letrec
	((cc
	  (lambda(a b)
	    (cons a b)))
	 (sel
	  (lambda(lat col)
	    (cond
	     ((null? lat)
	      (col '() '()))
	     ((= (remainder (car lat)2)0)
	      (sel (cdr lat)(lambda(c d)
		(col (cons (car lat)c) d))))
	     ((= (remainder (car lat)2)1)
	      (sel (cdr lat)(lambda(c d)
		(col c (cons (car lat) d)))))))))
      (sel lat cc))))
		    
			  
		
(define (generate-one-element-at-a-time lst)
 ;; Hand the next item from a-list to 'return' or an end-of-list marker
  (define (control-state return)
    (for-each
     (lambda (element)
       (call/cc
	(lambda (resume-here)
	  ;; Grab the current continuation
	  (set! control-state resume-here) ;; !!!
	  (return element))))
     lst)
    (return 'end))
  (define (generator)
    (call/cc control-state))
 ;; Return the generator
  generator)
(define generate-digit
  (generate-one-element-at-a-time '(0 1 2)))

(define Y
  (lambda(la)
    (lambda(f)
      (f f))
    (lambda(f)
      (la (lambda(x) ((f f ) x))))))

(define len
  (lambda(lat)
    ((Y (lambda(length)
	 (lambda(x)
	   (cond
	    ((null? x) 0)
	    (else
	     (add1 (length (cdr x))))))))
     lat)))

(define lwp-list '())

(define lwp
  (lambda (thunk)
    (set! lwp-list (append lwp-list (list thunk))))) 

(define start
  (lambda ()
    (let ([p (car lwp-list)])
      (set! lwp-list (cdr lwp-list))
      (p)))) 

(define pause
  (lambda ()
    (call/cc
     (lambda (k)
       (lwp (lambda () (k #f)))
       (start))))) 

(lwp (lambda () (let f () (pause) (display "h") (f))))
(lwp (lambda () (let f () (pause) (display "e") (f))))
(lwp (lambda () (let f () (pause) (display "y") (f))))
(lwp (lambda () (let f () (pause) (display "!") (f))))
(lwp (lambda () (let f () (pause) (newline) (f))))
	     


(define (new-if predicate then-clause else-clause)
  (cond
   (predicate then-clause)
   (else else-clause)))

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
	  guess
	  (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs( - (square guess )x )) 0.001))

(define (square x )
  (* x  x))
  

(define (f n)
  (cond
   (( = n 0)0)
   ((= n 1)1)
   ((= n 2)2)
   (else
    (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3)))))))

(define getf
  (lambda(n)
    (let cao ([i 1])
      (cond
       (( > i n)'())
       (else
	(cons (f i) (cao (add1 i))))))))

(define f2
  (lambda(n)
    (letrec
	((inner
	  (lambda(a b c n)
	    (cond
	     ((= n 0)a)
	     ((= n 1)b)
	     ((= n 2)c)
	     (else
	      (inner b c (+ c (* 2 b) (* 3 a)) (sub1 n)))))))
      (inner 0 1 2 n))))

(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

(define (expt2 b n)
  (cond
   ((= n 0)1)
   ((= (remainder n 2)0)(square (expt2 b (/ n 2))))
   (else
    (* b (expt2 b (- n 1))))))

(define square
  (lambda(n)
    (* n n )))

(define (one two)
  (display 1)
  (newline)
  (set! two (call/cc two))
  (display 2)
  (newline)
  (set! two (call/cc two))
  (display 3)
  (newline)
  (set! two (call/cc two)))
(define (two one)
  (display "hello")
  (newline)
  (set! one (call/cc one))
  (display "hi")
  (newline)
  (set! one (call/cc one))
  (display "hhh")
  (newline)
  (set! one (call/cc one)))

((lambda (pair)
   (begin
     (display (car pair))
     ((cdr pair)
      (cons (+ 1 (car pair)) 
            (cdr pair)))))
 (call/cc (lambda (k) 
            (cons 0 k))))

((cdr
  (or (call/cc (lambda (cc) (cons 2 (lambda () (cc #f)))))
      (cons 3 (lambda() (+ 3 2))))))

(define (Y f)
  ((lambda (u) (u (lambda (x) (lambda (n) ((f (u x)) n)))))
   (call/cc (call/cc (lambda (x) x)))))

(define lazy
  (lambda (t)
    (let ([val #f] [flag #f])
      (lambda ()
	(if (not flag)
	    (begin (set! val (t))
		   (set! flag #t)))
	val))))

(define p
  (lazy (lambda ()
	  (display "Ouch!")
	  (newline)
	  "got me")))

(define make-stack
  (lambda ()
    (let ([ls '()])
      (lambda (msg . args)
	(cond
	 [(eqv? msg 'empty?) (null? ls)]
	 [(eqv? msg 'push!) (set! ls (cons (car args) ls))]
	 [(eqv? msg 'top) (car ls)]
	 [(eqv? msg 'pop!) (set! ls (cdr ls))]
	 [else "oops"])))))

(define-syntax abc
  (syntax-rules()
    [(_ ((x e) ...) b1 b2 ...)
     ((lambda (x ...) b1 b2 ...) e ...)]))

(define lots
  (lambda(m)
    (cond
     ((zero? m) (quote ()))
     (else
      (kons (quote egg)
	    (lots (sub1 m)))))))
(define add-at-end
  (lambda(l)
    (cond
     ((null? (kdr l))
      (kons (kar l)
	     (kons (quote egg)
		   (quote ()))))
     (else
      (kons (kar l)
	     (add-at-end (kdr l)))))))
(define add-at-end-too
  (lambda(l)
    (letrec
	((A (lambda(ls)
	      (cond
	       ((null? (kdr ls))
		(set-kdr ls (kons (quote egg) (quote ()))))
	       (else
		(A (kdr ls)))))))
      (A l)
      )))
(define kar
  (lambda(c)
    (c (lambda(s a d) a))))
(define kdr
  (lambda(c)
    (c (lambda(s a d) d))))
(define kons
  (lambda (a d)
    (let ((c (bons a)))
      (set-kdr c d)
      c)))
(define bons
  (lambda(kar)
    (let ((kdr (quote ())))
      (lambda(selector)
	(selector
	 (lambda(x) (set! kdr x))
	 kar
	 kdr)))))
(define set-kdr
  (lambda (c x)
    ((c (lambda (s a d)s)) x)))
(define disppp
  (lambda (k)
    (cond
     ((null? k)'fin)
     (else
      (display (kar k))
      (display " ")
      (disppp (kdr k))))))
(define eklist?
  (lambda (ls1 ls2)
    (cond
     ((null? ls1) (null? ls2))
     ((null? ls2) #f)
     (else
      (and (eq? (kar ls1) (kar ls2))
	   (eklist? (kdr ls1) (kdr ls2)))))))
(define same?
  (lambda (c1 c2)
    (let ((t1 (kdr c1))
	  (t2 (kdr c2)))
      (set-kdr c1 1)
      (set-kdr c2 2)
      (let ((v ( = (kdr c1) (kdr c2))))
	(set-kdr c1 t1)
	(set-kdr c2 t2)
	v))))
(define last-kons
  (lambda (ls)
    (cond
     ((null? (kdr ls)) ls)
     (else
      (last-kons (kdr ls))))))
(define lenkth
  (lambda (l)
    (cond
     ((null? l)0)
     (else
      (add1 (lenkth (kdr l)))))))
(define finite-lenkth
  (lambda (p)
    (lambda (p)
      (call/cc
       (lambda (infinite)
	 (letrec
	     ((C (lambda (p q)
		   (cond
		    ((same? p q)
		     (infinite #f))
		    ((null? q)0)
		    ((null? (kdr q)) 1)
		    (else
		     (+ (C (sl p) (qk q))
			2)))))
	      (qk (lambda (x) (kdr (kdr x))))
	      (sl (lambda (x) (kdr x))))
	   (cond
	    ((null? p) 0)
	    (else
	     (add1 (C p (kdr p)))))))))))
(define six-layers
  (lambda (p)
    (cons
     (cons
      (cons
       (cons
	(cons
	 (cons p (quote ()))
	 (quote ()))
	(quote ()))
       (quote ()))
      (quote ()))
     (quote ()))))

(define toppings #f)
(define deepB
  (lambda (m)
    (cond
     ((zero? m)
      (call/cc
       (lambda (jump)
	 (set! toppings jump)
	 (quote pizza))))
     (else
      (cons (deepB (sub1 m))
	    '())))))

(define evens-only
  (lambda(l col)
    (cond
     ((null? l)
      (col '() 1 0))
     ((atom? (car l))
      (cond
       ((even? (car l))
	(evens-only (cdr l)
		    (lambda(newl p s)
		      (col (cons (car l) newl)
			   (* (car l) p)
			   s))))
       (else
	(evens-only (cdr l)
		    (lambda(newl p s)
		      (col newl p (+ (car l) s)))))))
     (else
      (evens-only (car l) (lambda(al ap as)
			    (evens-only (cdr l)
					(lambda(dl dp ds)
					  (col (cons al dl)
					       (* ap dp)
					       (+ as ds))))))))))
(define (first ls)
  (car ls))
(define (second ls)
  (cadr ls))
(define build list)
(define lookup-in-entry
  (lambda(name entry entry-f)
    (lookup-in-entry-help
     name
     (first entry)
     (second entry)
     entry-f)))
(define lookup-in-entry-help
  (lambda(name names values entry-f)
    (cond
     ((null? names) (entry-f name))
     ((eq? (car names) name)
      (car values))
     (else
      (lookup-in-entry-help
       name
       (cdr naems)
       (cdr values)
       entry-f)))))
(define extend-table cons)
(define lookup-in-table
  (lambda(name table table-f)
    (cond
     ((null? table)(table-f name))
     (else
      (lookup-in-entry
       name
       (car table)
       (lambda(name)
	 (lookup-in-table
	  name
	  (cdr table)
	  table-f)))))))

(define divide-samelist
  (lambda(ls)
  (letrec
      ((A (lambda(ls k)
	    (cond
	     ((null? ls) (k '() '()))
	     ((null? (cdr ls))(k ls '()))
	     ((eq? (car ls) (cadr ls))
	      (A (cdr ls)
		 (lambda(s l)
		   (k (cons (car ls) s)
		      l))))
	     (else
	      (k (cons (car ls) '())
		 (cdr ls))))))
       (B (lambda(ls)
	    (let ((a (A ls cons)))
	      (cond
	       ((null? (car a))
		(car a))
	       (else
		(cons (car a) (B(cdr a)))))))))
    (B ls))))
			   

(define turn-to
  (lambda(ls)
    (eval `(lambda(x) ,ls))))

(define fab-vec (make-vector 100 0))

(define fabb
  (lambda(n)
    (cond
     ((= n 1) 1)
     ((= n 2) 1)
     ((zero? (vector-ref fab-vec n))
      (let ([a (fabb (- n 1))] [b (fabb (- n 2))])
	(vector-set! fab-vec (- n 1) a)
	(vector-set! fab-vec (- n 2) b)
	(vector-set! fab-vec n (+ a b))
	(+ a b)))
     (else
      (vector-ref fab-vec n)))))

(define fab
  (lambda(n)
    (cond
     ((= n 1) 1)
     ((= n 2) 1)
     (else
      (+ (fab (- n 1))(fab (- n 2)))))))

(define lazy
  (lambda (t)
    (let ([var #f] [once #t])
      (lambda()
	(if once
	    (begin (set! var (t))
		   (set! once #f)
		   var))
	var))))


(define fab-vec (make-vector 100 0))

(define fabb
  (lambda(n)
    (cond
     ((= n 1) 1)
     ((= n 2) 1)
     ((zero? (vector-ref fab-vec n))
      (let ([a (fabb (- n 1))] [b (fabb (- n 2))])
	(vector-set! fab-vec (- n 1) a)
	(vector-set! fab-vec (- n 2) b)
	(vector-set! fab-vec n (+ a b))
	(+ a b)))
     (else
      (vector-ref fab-vec n)))))

(let loop ((i 1))
  (cond
   ((= i 50))
   (else
    (vector-set! fab-vec i (lazy(fabb i)))
    (loop (add1 i)))))

(define fabb
  (lambda(n)
    (cond
     ((= n 1) (lambda() 1))
     ((= n 2) (lambda() 1))
     (else
      (lambda()
	  (+ ((vector-ref fab-vec (- n 1)))
	     ((vector-ref fab-vec (- n 2)))))))))

(define-syntax defun
  (syntax-rules()
    ((_ (fst a ...)
       body ...)
     (define fst
       (lambda(a ...)
	 body ...)))))

(defun (a x y) (+ x y))
(defun (a x y) (+ x y) 1 2)
(define (a x y) (+ x y))

(define-syntax lett
  (syntax-rules()
    [(_ ((x1 e) ...)
	b ...)
     ((lambda(x1 ...)
	b ...) e ...)]))
(define-syntax let**
  (syntax-rules()
    [(_ ()
	b1 b2 ...)
     (let ()
       b1 b2 ...)]
    [(_ ((x e))
	b1 b2 ...)
     (let ((x e))
       b1 b2 ...)]
    [(_ ((x1 e1) (x2 e2) ...)
	b1 b2 ...)
     (let ((x1 e1))
       (let** ((x2 e2) ...)
	      b1 b2 ...))]))
(define-syntax when
  (syntax-rules()
    [(_ val b1 b2 ...)
     (if val
	 ((lambda()
	    b1 b2 ...))
	 #f)]))
(define-syntax unless
  (syntax-rules()
    [(_ val b1 b2 ...)
     (when val b1 b2 ...)]))

(define-syntax letre
  (syntax-rules()
    [(_ ((x1 e1) ...)
	b1 b2 ...)
     (let ((x1 0) ...)
       (set! x1 e1) ...
       b1 b2 ...)]))

(define-syntax mylet
  (syntax-rules()
    [(_ ((x1 e1) ...)
	b1 b2 ...)
     ((lambda(x1 ...)
	b1 b2 ...) e1 ...)]
    [(_ name ((x1 e1) ...)
	b1 b2 ...)
     (let ((name #f))
       (set! name
	     (lambda(x1 ...) b1 b2 ...))
       (name e1 ...))]))

(define-syntax ors ; incorrect!
  (syntax-rules ()
    [(_) #f]
    [(_ e1 e2 ...)
     (let ([t e1])
       (if t t (ors e2 ...)))]))

(letrec ([even?
	  (lambda (x)
	    (or (= x 0)
		 (odd? (- x 1))))]
	 [odd?
	  (lambda (x)
	    (and (not (= x 0))
		 (even? (- x 1))))])
  (even? 200000000000000000000))

(define retry #f)
(define new #f)
(define factorial
  (lambda (x)
    (if (= x 0)
	(call/cc (lambda (k) (set! retry k) 1))
	(* x (factorial (- x 1))))))

(letrec ([f (lambda (x) (cons 'a x))]
	 [g (lambda (x) (cons 'b (f x)))]
	 [h (lambda (x) (g (cons 'c x)))])
  (cons 'd (h '())))

(letrec ([f (lambda (x k) (k (cons 'a x)))]
	 [g (lambda (x k)
	      (f x (lambda (v) (k (cons 'b v)))))]
	 [h (lambda (x k) (g (cons 'c x) k))])
  (h '() (lambda (v) (cons 'd v))))

(define (f x) (cons x '()))
(cons 'a (f 'b))

(f 'b (lambda(k) (cons 'a k)))
(define (f x k) (k (cons x '())))


(define product
  (lambda (ls)
    (call/cc
     (lambda (break)
       (let f ([ls ls])
	 (cond
	  [(null? ls) 1]
	  [(= (car ls) 0) (break 0)]
	  [else (* (car ls) (f (cdr ls)))]))))))

(define product
  (lambda(ls k)
    (let f ([ls ls] [k1 k])
      (cond
       [(null? ls) (k1 1)]
       [(= (car ls) 0) (k 0)]
       [else
	(f (cdr ls)
	   (lambda(m)
	     (k1 (* m (car ls)))))]))))

(define factorial
  (lambda (x)
    (if (= x 0)
	(call/cc (lambda (k) (set! retry k) 1))
	(* x (factorial (- x 1))))))

(define re #f)
(define fact
  (lambda(x ke)
    (let f ((y x) (k1 ke))
      (cond
       ((= y 0)
	(set! re k1)
	(k1 1))
       (else
	(f (- y 1) (lambda(k)
		     (k1 (* k y)))))))))

(define I (lambda(x) x))

(define reciprocals
  (lambda (ls)
    (call/cc
     (lambda (k)
       (map (lambda (x)
	      (if (= x 0)
		  (k "zero found")
		  (/ 1 x)))
	    ls)))))
      
(define reciprocals
  (lambda (ls k)
    (let ((k1 k))
      (map ((lambda(k)
	      (lambda(x)
		(if (= x 0)
		    (k "cao")
		    (/ 1 x)))) (lambda(k1) k))
	   ls))))

(library (grades)
  (export gpa->grade gpa)
  (import (rnrs))
  (define in-range?
    (lambda (x n y)
      (and (>= n x) (< n y))))
  (define-syntax range-case
    (syntax-rules (- else)
      [(_ expr ((x - y) e1 e2 ...) ... [else ee1 ee2 ...])
       (let ([tmp expr])
	 (cond
	  [(in-range? x tmp y) e1 e2 ...]
	  ...
	  [else ee1 ee2 ...]))]
      [(_ expr ((x - y) e1 e2 ...) ...)
       (let ([tmp expr])
	 (cond
	  [(in-range? x tmp y) e1 e2 ...]
	  ...))]))
  (define letter->number
    (lambda (x)
      (case x
	[(a) 4.0]
	[(b) 3.0]
	[(c) 2.0]
	[(d) 1.0]
	[(f) 0.0]
	[else (assertion-violation 'grade "invalid letter grade" x)])))
  (define gpa->grade
    (lambda (x)
      (range-case x
		  [(0.0 - 0.5) 'f]
		  [(0.5 - 1.5) 'd]
		  [(1.5 - 2.5) 'c]
		  [(2.5 - 3.5) 'b]
		  [else 'a])))
  (define-syntax gpa
    (syntax-rules ()
      [(_ g1 g2 ...)
       (let ([ls (map letter->number '(g1 g2 ...))])
	 (/ (apply + ls) (length ls)))])))

(define list-copy
  (lambda (ls)
    (if (null? ls)
	'()
	(cons (car ls)
	      (list-copy (cdr ls))))))

(define list-cppy
  (lambda (ls k)
    (if (null? ls)
	(k '())
	(list-cppy
	 (cdr ls) (lambda(m) (k (cons (car ls) m)))))))

(define memv
  (lambda (x ls)
    (cond
     [(null? ls) #f]
     [(eqv? (car ls) x) ls]
     [else (memv x (cdr ls))])))

(define mymv
  (lambda(x ls k)
    (cond
     [(null? ls) (k #f)]
     [(eqv? (car ls) x) (k ls)]
     [else (mymv x (cdr ls) k)])))

(define remv
  (lambda (x ls)
    (cond
     [(null? ls) '()]
     [(eqv? (car ls) x) (remv x (cdr ls))]
     [else (cons (car ls) (remv x (cdr ls)))])))

(define rymv
  (lambda (x ls k)
    (cond
     [(null? ls) (k '())]
     [(eqv? (car ls) x) (rymv x (cdr ls) k)]
     [else (rymv x (cdr ls) (lambda(m) (k (cons (car ls) m))))])))

(define tree-copy
  (lambda (tr)
    (if (not (pair? tr))
	tr
	(cons (tree-copy (car tr))
	      (tree-copy (cdr tr))))))

(define tree-cypy
  (lambda(tr k)
    (cond
     [(not (pair? tr))
      (k tr)]
     [else
      (tree-cypy (car tr)
		 (lambda(m)
		   (k (cons m (tree-cypy (cdr tr) I)))))])))

(define abs-all
  (lambda (ls)
    (if (null? ls)
	'()
	(cons (abs (car ls))
	      (abs-all (cdr ls))))))

(define ays-all
  (lambda (ls k)
    (if (null? ls)
	(k '())
	(ays-all (cdr ls)
		 (lambda(m)
		   (k (cons (abs (car ls)) m)))))))

(define divideby3
  (lambda(ls)
    (call/cc
     (lambda(break)
       (map (lambda(x)
	      (if (= x 0)
		  (break 'cao)
		  (/ 3 x)))
	    ls)))))

(define divideby3
  (lambda(ls k)
    (lambda()
      (map (lambda(x)
	     (if (= x 0)
		 (k 'cao)
		 (/ 3 x)))
	   ls))))


(define first car)
(define second cadr)
(define lookup-in-entry
  (lambda(name entry entry-f)
    (lookup-in-entry-help
     name
     (first entry)
     (second entry)
     entry-f)))
(define lookup-in-entry-help
  (lambda(name names values entry-f)
    (cond
     ((null? names) (entry-f name))
     ((eq? (car names) name)
      (car values))
     (else
      (lookup-in-entry-help
       name
       (cdr names)
       (cdr values)
       entry-f)))))
(define extend-table cons)
(define lookup-in-table
  (lambda(name table table-f)
    (cond
     ((null? table)(table-f name))
     (else
      (lookup-in-entry
       name
       (car table)
       (lambda(name)
	 (lookup-in-table
	  name
	  (cdr table)
	  table-f)))))))

(define atom-to-action
  (lambda(e)
    (cond
     ((number? e) *const)
     ((eq? e #t) *const)
     ((eq? e #f) *const)
     ((eq? e 'cons) *const)
     ((eq? e 'car) *const)
     ((eq? e 'cdr) *const)
     ((eq? e 'null?) *const)
     ((eq? e 'eq? ) *const)
     ((eq? e 'atom?) *const)
     ((eq? e 'zero?) *const)
     ((eq? e 'add1) *const)
     ((eq? e 'sub1) *const)
     ((eq? e 'number?) *const)
     (else
      *identifier))))
(define list-to-action
  (lambda(e)
    (cond
     ((atom? (car e))
      (cond
       ((eq? (car e) 'quote)
	*quote)
       ((eq? (car e) 'lambda)
	*lambda)
       ((eq? (car e) 'cond)
	*cond)
       (else
	*application)))
      (else
       *application))))
(define expression-to-action
  (lambda(e)
    (cond
     ((atom? e) (atom-to-action e))
     (else
      (list-to-action e)))))

(define build list)
(define (atom? a)
  (and (not (pair? a)) (not (null? a))))

(define value
  (lambda(e)
    (meaning e '())))
(define meaning
  (lambda(e table)
    ((expression-to-action e) e table)))

(define *const
  (lambda(e table)
    (cond
     ((number? e) e)
     ((eq? e #t) #t)
     ((eq? e #f) #f)
     (else
      (build 'primitive e)))))

(define *quote
  (lambda(e table)
    (cadr e)))

(define initial-table
  (lambda(name)
    (car '())))
(define *identifier
  (lambda(e table)
    (lookup-in-table e table initial-table)))

(define *lambda
  (lambda(e table)
    (build 'non-primitive
	   (cons table (cdr e)))))
(define table-of car)
(define formals-of cadr)
(define body-of caddr)
(define build list)

(define else?
  (lambda(x)
    (cond
     ((atom? x) (eq? x 'else))
     (else #f))))
(define question-of car)
(define answer-of cadr)
(define evcon
  (lambda(lines table)
    (cond
     ((else? (question-of (car lines)))
      (meaning (answer-of (car lines))
	       table))
     ((meaning (question-of (car lines))
	       table)
      (meaning (answer-of (car lines))
	       table))
     (else
      (evcon (cdr lines) table)))))
(define *cond
  (lambda(e table)
    (evcon (cond-lines-of e) table)))
(define cond-lines-of cdr)

(define evlis
  (lambda(args table)
    (cond
     ((null? args) '())
     (else
      (cons (meaning (car args) table)
	    (evlis (cdr args) table))))))
(define *application
  (lambda(e table)
    (apply
     (meaning (function-of e) table)
     (evlis (arguments-of e) table))))
(define function-of car)
(define arguments-of cdr)

(define primitive?
  (lambda(l)
    (eq? (car l) 'primitive)))
(define non-primitive?
  (lambda(l)
    (eq? (car l) 'non-primitives)))

(define apply
  (lambda(fun vals)
    (cond
     ((primitive? fun)
      (apply-primitive
       (cadr fun) vals))
     ((non-primitve? fun)
      (apply-closure
       (cadr fun) vals)))))

(define apply-primitive
  (lambda(name vals)
    (cond
     ((eq? name 'cons)
      (cons (car vals) (cadr vals)))
     ((eq? name 'car)
      (car (car vals)))
     ((eq? name 'cdr)
      (cdr (car vals)))
     ((eq? name 'null?)
      (null? (car vals)))
     ((eq? name 'eq?)
      (eq? (car vals) (cdr vals)))
     ((eq? name 'atom?)
      (:atom? (car vals)))
     ((eq? name 'zero?)
      (zero? (car vals)))
     ((eq? name 'add1)
      (add1 (car vals)))
     ((eq? name 'sub1)
      (sub1 (car vals)))
     ((eq? name 'number?)
      (number? (car vals))))))
(define :atom?
  (lambda(x)
    (cond
     ((atom? x) #t)
     ((null? x) #f)
     ((eq? (car x) 'primitive)
      #t)
     ((eq? (car x) 'non-primitive)
      #t)
     (else #f))))
(define apply-closure
  (lambda(closure vals)
    (meaning (body-of closure)
	     (extend-table
	      (new-entry
	       (formals-of closure)
	       vals)
	      (table-of closure)))))

(define fab
  (lambda (n k)
    (cond
     ((= n 1) (k 1))
     ((= n 2) (k 1))
     (else
      (k (+ (fab (- n 1) k)
	    (fab (- n 2) k)))))))

(define fib
  (lambda (n k)
    (cond
     ((= n 1) (k 1))
     ((= n 2) (k 1))
     (else
      (fib (- n 1)
	   (lambda (k2)
	     (fib (- n 2) (lambda(k3)(k(+ k3 k2))))))))))

(define fib
  (lambda (n k)
    (cond
     ((= n 1) (k 1 0))
     ((= n 2) (k 1 0))
     (else
      (fib (- n 1)
	   (lambda (k2 k3)
	     (fib (- n 2) (lambda (x y)(k (+ k2 k3) (+ x y))))))))))
(define fabio
  (lambda (n)
    (cond
     ((= n 1) 1)
     ((= n 2) 1)
     (else
      (+ (fabio (- n 1)) (fabio (- n 2)))))))

;; not 4 5
;; at least 8 bit

(define my-transcoder (make-transcoder (latin-1-codec) (eol-style crlf) (error-handling-mode ignore)))

(define new (open-file-input-port
	     "1.txt"
	     (file-options no-create)
	     (buffer-mode block)
	     ))

(define fin (call-with-port new
		(lambda (p)
		  (call-with-bytevector-output-port
		   (lambda (k)
		     (let f ((a (get-u8 p)))
		       (cond
			((eof-object? a))
			((eqv? (integer->char a) #\newline)
			 (f (get-u8 p)))
			(else
			 (put-u8 k a) (f (get-u8 p))))))))))

(close-port new)

(do ((i 0 (+ i 1)))
    ((= i 150))
  (bytevector-u8-set! fin i
		      (- (bytevector-u8-ref fin i)
			 (char->integer #\0))))

(define fin-list (bytevector->u8-list fin))

(define new-fin-list
  (let loop ([ls fin-list])
    (cond
     ((null? ls) '())
     (else
      (cons (let f ((i 0) (ls ls))
	      (cond
	       ((= i 3) '())
	       (else (cons (car ls)
			   (f (+ i 1) (cdr ls))))))
	    (loop (cdddr ls)))))))

(define get-num
  (lambda (a)
    (reverse
     (let f ([a a])
       (cond
        ((zero? a)'())
	(else
	 (cons (remainder a 10)
	       (f (/(- a(remainder a 10))10)))))))))

(define in-order?
  (lambda (order num-list)
    (cond
     ((null? (cdr order))
      (member (car order) num-list))
     (else
      (let* ([fst (car order)]
	     [nu-list (member fst num-list)])
	(if (not nu-list)
	    #f
	    (in-order? (cdr order) nu-list)))))))

(define ass-in-order?
  (lambda (order-list num-list)
    (cond
     ((null? order-list) #t)
     ((in-order? (car order-list) num-list)
      (ass-in-order? (cdr order-list) num-list))
     (else
      #f))))

(time(let f ((i 10000000))
  (let ([n-list (get-num i)])
    (cond
     ([= i 99999999] '())
     ((ass-in-order? new-fin-list n-list)
      (cons i (f (+ i 1))))
     (else
      (f (+ i 1)))))))

(define sum 0)
(define N (+ 50 1))
(define rest (- (* N N 3) 2))
(define vec*
  (lambda (c1 c2)
    (+(* (car c1) (car c2)) (* (cdr c1) (cdr c2)))))

(let f ((i 0))
  (cond
   ((= i N))
   (else
    (let g ((j 0))
      (cond
       ((= j N) (f (+ i 1)))
       (else
	(let a ((k 0))
	  (cond
	   ((= k N) (g (+ j 1)))
	   (else
	    (let b ((m 0))
	      (cond
	       ((= m N) (a (+ k 1)))
	       (else
		(let ([v1 (cons i j)]
		      [v2 (cons k m)]
		      [v3 (cons (- k i) (- m j))])
		  (if (or (zero? (vec* v1 v2)) (zero? (vec* v1 v3)) (zero? (vec* v2 v3)))
		      (set! sum (+ 1 sum)))
		  (b (+ m 1)))))))))))))))

(define get-num
  (lambda (a)
    (reverse
     (let f ([a a])
       (cond
        ((zero? a)'())
	(else
	 (cons (remainder a 10)
	       (f (/(- a(remainder a 10))10)))))))))

(define MAX 10000000)

(define find
  (lambda (n)
    (cond
     ((= n 89) #t)
     ((= n 1) #f)
     (else
      (let* ([ls (get-num n)]
	     [new (fold-left (lambda(x y) (+ x (expt y 2))) 0 ls)])
	(find new))))))

(define accu 0)

(time(let f ([i 1])
  (cond
   ((= i MAX))
   ((eq? (find i) #t) (set! accu (+ 1 accu)) (f (+ i 1)))
   ((eq? (find i) #f) (f (+ i 1))))))
   
(define get-num
  (lambda (a)
    (reverse
     (let f ([a a])
       (cond
        ((zero? a)'())
	(else
	 (cons (remainder a 10)
	       (f (/(- a(remainder a 10))10)))))))))

(define get-num-rev
  (lambda (a)
    (let f ([a a])
      (cond
       ((zero? a)'())
       (else
	(cons (remainder a 10)
	      (f (/(- a(remainder a 10))10))))))))

(define find
  (lambda (n)
    (let f ([n (+ n (fold-left
		     (lambda (x y) (+ (* 10 x) y)) 0 (get-num-rev n)))]
	    [i 1])
      (cond
       ((= i 51) #f)
       ((equal? (get-num n) (get-num-rev n))
	(+ i 1))
       (else
	(let ([new (+ n (fold-left
			 (lambda (x y) (+ (* 10 x) y)) 0 (get-num-rev n)))])
	  (f new (+ i 1))))))))

(let f ([i 1] [accu 0])
  (cond
   ((= i 10000) accu)
   ((find i) (f (+ i 1) (+ accu 1)))
   (else
    (f (+ i 1) accu))))
  
    
;;1_2_3_4_5_6_7_8_9_0
;; 9*2 = 18 10 * 2 = 20
;;100000000 ~ 999999999

(define get-num
  (lambda (a)
    (reverse
     (let f ([a a])
       (cond
        ((zero? a)'())
	(else
	 (cons (remainder a 10)
	       (f (/(- a(remainder a 10))10)))))))))

(define suo
  (lambda (ls)
    (cond
     ((null? (cdr ls)) ls)
     (else
      (cons (car ls) (suo (cddr ls)))))))

(define satis?
  (lambda (n)
    (let ((num-list (get-num (expt n 2))))
      (cond
       ((= ( length num-list) 19)
	(let ((new-list (suo num-list)))
	  (cond
	   ((equal? new-list '(1 2 3 4 5 6 7 8 9 0))
	    #t)
	   (else #f))))
       (else #f)))))
	   

(let f ([i 1010101010])
  (cond
   ((= i 1389026624) #f)
   ((and (zero? (remainder i 10))
	 (satis? i))
    i)
   (else
    (f (+ i 1)))))

(define make-matrix
  (lambda (rol col)
    (let ([tmp (make-vector rol)])
      (let f ([i 0])
	(cond
	 ((= i col) tmp)
	 (else
	  (vector-set! tmp i (make-vector col))
	  (f (+ i 1))))))))

(define matrix-ref
  (lambda (mat row col)
    (vector-ref (vector-ref mat row)  col )))

(define matrix-set!
  (lambda (mat row col val)
    (vector-set! (vector-ref mat row) col val)))

(define n 15)

(define matt (make-matrix n n))
;;-------------------------------------------------------------------
(define prime (make-vector 1000 1))

(vector-set! prime 1 0)

(time(let f ([i 1])
       (cond
	((= i 1000))
	((= (vector-ref prime i) 1)
	 (let g ([j (* 2 i)])
	   (cond
	    ((>= j 1000) (f (+ i 1)))
	    (else
	     (vector-set! prime j 0)
	     (g (+ j i))))))
	(else
	 (f (+ i 1))))))

(do ((i 1 (+ i 1)))
    ((= i 1000))
  (if (= 1(vector-ref prime i))
      (begin (display i)(display #\space ))))

(define prime?
  (lambda (n)
    (= (vector-ref prime n) 1)))
;;--------------------------------------------------------------

(define k 1)

(let f ([p 0])
  (let ([max (quotient n 2)])
    (cond
     ((> p max))
     (else
      (let f ([i p])
	(when (<= i (- n 1 p))
	  (matrix-set! matt p i (+ 1 (* n n) (- k)))
	  (set! k (+ k 1))
	  (f (+ i 1))))
      (let f ([i (+ p 1)])
	(when (<= i (- n 1 p))
	  (matrix-set! matt i (- n 1 p) (+ 1 (* n n)(- k)))
	  (set! k (+ k 1))
	  (f (+ i 1))))
      (let f ([i (- n 1 p 1)])
	(when (>= i p)
	  (matrix-set! matt (- n 1 p) i (+ 1 (* n n) (- k)))
	  (set! k (+ k 1))
	  (f (- i 1))))
      (let f ([i (- n 1 p 1)])
	(when (>= i (+ p 1))
	  (matrix-set! matt i p (+ 1 (* n n) (- k)))
	  (set! k (+ k 1))
	  (f (- i 1))))
      (f (+ p 1))))))

(define cross-num (- (* 2 n) 1))

(define find-prime
  (+ (let f ([i 0][j 0] [acc 0])
       (cond
	((= i n) acc)
	((prime? (matrix-ref matt i j))
	 (f (+ i 1) (+ j 1) (+ acc 1)))
	(else
	 (f (+ i 1) (+ j 1) acc))))
     (let f ([i 0][j (- n 1)] [acc 0])
       (cond
	((= i n) acc)
	((prime? (matrix-ref matt i j))
	 (f (+ i 1) (- j 1) (+ acc 1)))
	(else
	 (f (+ i 1) (- j 1) acc))))))

(exact->inexact (/ find-prime cross-num))

(begin
(define n 9999)

(define matt (make-matrix n n))

(define k 1)

(let f ([p 0])
  (let ([max (quotient n 2)])
    (cond
     ((> p max))
     (else
      (let f ([i p])
	(when (<= i (- n 1 p))
	  (matrix-set! matt p i (+ 1 (* n n) (- k)))
	  (set! k (+ k 1))
	  (f (+ i 1))))
      (let f ([i (+ p 1)])
	(when (<= i (- n 1 p))
	  (matrix-set! matt i (- n 1 p) (+ 1 (* n n)(- k)))
	  (set! k (+ k 1))
	  (f (+ i 1))))
      (let f ([i (- n 1 p 1)])
	(when (>= i p)
	  (matrix-set! matt (- n 1 p) i (+ 1 (* n n) (- k)))
	  (set! k (+ k 1))
	  (f (- i 1))))
      (let f ([i (- n 1 p 1)])
	(when (>= i (+ p 1))
	  (matrix-set! matt i p (+ 1 (* n n) (- k)))
	  (set! k (+ k 1))
	  (f (- i 1))))
      (f (+ p 1))))))

(define cross-num (- (* 2 n) 1))

(define find-prime
  (+ (let f ([i 0][j 0] [acc 0])
       (cond
	((= i n) acc)
	((prime? (matrix-ref matt i j))
	 (f (+ i 1) (+ j 1) (+ acc 1)))
	(else
	 (f (+ i 1) (+ j 1) acc))))
     (let f ([i 0][j (- n 1)] [acc 0])
       (cond
	((= i n) acc)
	((prime? (matrix-ref matt i j))
	 (f (+ i 1) (- j 1) (+ acc 1)))
	(else
	 (f (+ i 1) (- j 1) acc))))))

(exact->inexact (/ find-prime cross-num))
)

(define left-cross
  (lambda (n)
    (+ (* (- n 1) n) 1)))
;;form 1 to n

(define right-cross
  (lambda (n)
    (+ (* 4 (* n n)) 1)))
;;from 1 to (n-1)/2

(define cross-num
  (lambda (n)
    (- (* 2 n) 1)))
;;num of cross

(define prime?
  (lambda (n)
    (let ([max (inexact->exact (sqrt n))])
      (if (zero? (remainder n 2))
	  #f
	  (let f ([i 3])
	    (cond
	     ((> i max) #t)
	     ((zero? (remainder n i)) #f)
	     (else
	      (f (+ i 2)))))))))

(define find
  (lambda(n) 
    (/ (+
	(let f ([i 2][acc 0.0])
	  (cond
	   ((> i n) acc)
	   ((prime? (left-cross i))
	    (f (+ i 1) (+ acc 1)))
	   (else
	    (f (+ i 1) acc))))
	(let f ([i 1][acc 0])
	  (cond
	   ((> i (/ (- n 1) 2)) acc)
	   ((prime? (right-cross i))
	    (f (+ i 1) (+ acc 1)))
	   (else
	    (f (+ i 1) acc)))))
       (cross-num n))))
(let f ([i 25001])
  (let ([k (find i)])
    (cond
     ((< k 0.1) i)
     (else
      (display i)
      (display #\:)
      (display k)
      (newline)
      (f (+ i 2))))))
;;26591
;;26241

(define phi
  (lambda (n)
    (let f ([i 2][acc 0])
      (cond
       ((= i n) (/ n (+ acc 1)))
       ((= (gcd n i) 1)
	(f (+ i 1) (+ 1 acc)))
       (else
	(f (+ i 1) acc))))))
(define true-phi
  (lambda (n)
    (/ n (phi n))))

(time (let f ([i 2] [max-index 6] [max (phi 6)])
	(cond
	 ((= i 10000)
	  (display max-index)
	  (display #\:)
	  (display max))
	 (else
	  (let ([now (phi i)])
	    (cond
	     ((> now max)
	      (f (+ i 1) i now))
	     (else
	      (f (+ i 1) max-index max))))))))
;;2310: 77/16 <1~10000>
(time (phi 10000000))

(do ((i 2 (+ 1 i)))
    ((= i 101) )
  (display i)
  (display #\:)
  (display (inexact (phi i)))
  (newline))

(define a (make-vector 1000001 0))

(let f ((i 1))
  (cond
   ((= i 1000001))
   (else
    (vector-set! a i i)
    (f (+ i 1)))))

(let f ([i 2])
  (cond
   ((= i 1000001))
   ((= i (vector-ref a i))
    (let g ([j i])
      (cond
       ((>= j 1000001) (f (+ i 1)))
       (else
	(vector-set! a j (/ (* (vector-ref a j) (- i 1)) i))
	(g (+ j i))))))
   (else (f (+ i 1)))))

(time(do ((i 1 (+ i 1)))
    ((= i 1000001))
    (vector-set! a i (/ i (vector-ref a i)))))

(apply max (vector->list a))

(define lis (vector->list a))

(let f ((i 1))
  (cond
   ((= 17017/3072 (vector-ref a i))
    i)
   (else
    (f (+ i 1)))))
	
;;no 4 and 5

;;at least 8 bit

(define new (open-file-input-port "1.txt"
				  (file-options no-create)
				  (buffer-mode block)
				  (native-transcoder)))
(define my-transcoder (make-transcoder
		       (latin-1-codec)
		       (eol-style crlf)
		       (error-handling-mode ignore)))

(define str (open-string-input-port "1234567890"))

(call-with-port str (lambda(p)(let f ((a (get-char p))) (cond ((eof-object? a)) (else (display a) (f (get-char p)))))))

(define origin (make-bytevector 50 ))

(define newbyt (make-bytevector 10))

(define vec-por (open-bytevector-input-port origin))

(call-with-port vec-por (lambda (p) (let f ((a (get-u8 p))) (cond ((eof-object? a)) (else (display a) (f (get-u8 p)))))))

(define outvec (make-bytevector 150 ))


(define dis10
  (lambda(ls)
    (cond
     ((null? ls))
     (else
      (let loop ([n 10] [ato (car ls)])
	(cond
	 ((= n 1)
	  (display ato) (newline)
	  (dis10 (cdr ls)))
	 (else
	  (display ato) (loop (- n 1) ato))))))))

(dis10 '(1 2 3))


(do ([n 10 (- n 1)])
    ((= n 1) (display "hello"))
  (display n))


(call-with-port (open-file-input-port "12.txt" (file-options) (buffer-mode block) (native-transcoder))
		(lambda (ip)
		  (call-with-port #|(open-file-output-port "1.txt" (file-options no-fail)
							 (buffer-mode block)
		   (native-transcoder))|#
		   (current-output-port)
				  (lambda(op)
				    (do ([c (get-char ip) (get-char ip)])
					((eof-object? c))
				      (put-char op c))))))
