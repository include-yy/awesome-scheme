(define get-num
  (lambda (a)
    (reverse
     (let f ([a a])
       (cond
        ((zero? a)'())
	(else
	 (cons (remainder a 10)
	       (f (/(- a(remainder a 10))10)))))))))
;;(get-num 123456) => (1 2 3 4 5 6)

(define get-restlist ;;start at one
  (lambda (lat start)
    (cond
     ((>  start (length lat))#f)
     (else
      (cond
       ((= start 1)lat)
       (else
	(get-restlist (cdr lat) (sub1 start))))))))
;;(get-restlist '(1 2 3 4 5) 3) => (3 4 5)

(define make-singlelist
  (lambda (total signal)
    (cond
     ((= total 0)'())
     (else
      (cons signal (make-singlelist (sub1 total) signal))))))
;;(make-singlelist 5 1) => (1 1 1 1 1)

(define factor
  (lambda (n)
    (let fac
	[(n n)
	 (mul 1)]
      (cond
       ((= 1 n)mul)
       (else
	(fac (sub1 n) (* mul n)))))))

(define get-ncar
  (lambda(lat n)
    (if (> n (length lat))
	#f
	(cond
	 ((zero? n) '())
	 (else
	  (cons (car lat)
		(get-ncar (cdr lat) (sub1 n))))))))
;;(get-ncar '(1 1 1 1 1) 3) => (1 1 1)

(define cndr
  (lambda(lat n)
    (if (> n (length lat))
	#f
	(cond
	 ((zero? n) lat)
	 (else
	  (cndr (cdr lat) (sub1 n)))))))
;;(cndr '(1 1 1) 2) => (1)
     
(define list-div
  (lambda(lat n)
    (if (not (integer? (/ (length lat) n)))
	#f
	(cond
	 ((null? lat)'())
	 (else
	  (cons (get-ncar lat n)
		(list-div (cndr lat n) n)))))))
;;(list-div '(1 2 3 4 5 6) 2) => ((1 2) (3 4) (5 6))

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
(define divide-list2
  (lambda(ls)
    (letrec
	((A (lambda(now ls)
	       (cond
		((null? ls) now)
		((eq? (car now) (car ls))
		 (A (cons (car ls) now) (cdr ls)))
		(else
		 (cons now ls)))))
	 (B (lambda(ls)
	      (let ((a (A (list(car ls)) (cdr ls))))
		(cond
		 ((null? (cdr a))
		  (cons a '()))
		 (else
		  (cons (car a) (B (cdr a)))))))))
      (B ls))))
;;(divide-samelist '(1 1 2 2 3 3 4)) => ((1 1) (2 2) (3 3) (4))

(define (prime-accu n)
  (letrec
      ((A (lambda(acc a n1)
	    (cond
	     ((= n1 1) (add1 acc))
	     ((= (remainder n1 a)0)
	      (A (add1 acc) a (/ n1 a)))
	     (else
	      (* (add1 acc) (A  0 (add1 a) n1)))))))
    (A 0 2 n)))
;;(prime-accu 28) => 6
;;(28 -> 1 2 4 7 14 28)

#|
(define accumulate
  (lambda(combiner null-value term a next b)
    (cond
     ((= a b) null-value)
     (else
      (combiner (term a)
		(accumulate combiner null-value term (next a) next b))))))

(define accumulate
  (lambda(combiner null-value term a next b)
    (letrec ((A (lambda(start end)
		  (cond
		   ((= start end) null-value)
		   (else
		    (combiner (term start)
			      (A (next start) end)))))))
      (A a b))))
|#
(define filter
  (lambda(combiner null-value satis? term a next b terminate?)
    (letrec ((A (lambda(start end)
		  (cond
		   ((terminate? start end) null-value)
		   (else
		    (let ((trans (term start)))
		      (cond
		       ((satis? trans)
			(combiner trans (A (next start) end)))
		       (else
			(A (next start) end)))))))))
      (A a b))))
;;filter that is generally used for accumulate    
	
(define prime?
  (lambda (n)
    (cond
     ((<= n 0) #f)
     ((= n 1) #f)
     ((< n 4) #t)
     ((= (remainder n 2) 0) #f)
     ((< n 9) #t)
     ((= (remainder n 3) 0) #f)
     (else
      (let ([r (inexact->exact (floor (sqrt n)))])
	(let f ([i 5])
	  (cond
	   ((> i r) #t)
	   ((= (remainder n i) 0) #f)
	   ((= (remainder n (+ i 2)) 0) #f)
	   (else
	    (f (+ i 6))))))))))
;;E of sieve ---------------------------------------------------------------
(define range 10000000)
(define sievebound (quotient (- range 1) 2))
(define crosslimit (quotient (- (exact (floor (sqrt range))) 1) 2))

(define prime (make-bytevector sievebound 1))
(define prime-ref
  (lambda (pme i)
    (bytevector-u8-ref pme (- i 1))))
(define prime-set!
  (lambda (pme i arg)
    (bytevector-u8-set! pme (- i 1) arg)))

(let filt ([i 1])
  (cond
   ((> i sievebound))
   ((= (prime-ref prime i) 1)
    (let f ([j (* 2 i (+ i 1))])
      (cond
       ((> j sievebound) (filt (+ i 1)))
       (else
	(prime-set! prime j 0)
	(f (+ j (+ (* i 2) 1)))))))
   (else
    (filt (+ i 1)))))

(define prime-r?
  (lambda (n)
    (cond
     ((<= n 0) #f)
     ((= n 1) #f)
     ((= n 2) #t)
     ((= (remainder n 2) 0) #f)
     ((= (prime-ref prime (/ (- n 1) 2)) 1) #t)
     (else #f))))

(let pro ([i 1] [sum 2])
  (cond
   ((> i sievebound) sum)
   ((= (prime-ref prime i) 1)
    (pro (+ i 1) (+ sum (+ (* 2 i) 1))))
   (else
    (pro (+ i 1) sum))))
;;end -------------------------------------------------------------

(define make-matrix
  (lambda (m n)
    (let ([vec1 (make-vector m)])
      (let f ([i 0])
	(cond
	 ((>= i m) vec1)
	 (else
	  (vector-set! vec1 i (make-vector n))
	  (f (+ i 1))))))))
(define matrix-ref
  (lambda (mat i j)
    (let ([veci (vector-ref mat (- i 1))])
      (vector-ref veci (- j 1)))))
(define matrix-set!
  (lambda (mat i j val)
    (let ([veci (vector-ref mat (- i 1))])
      (vector-set! veci (- j 1) val))))
;;--------------------------------------------
(define get-num
  (lambda (a)
    (let* ([stra (number->string a)]
	   [clsa (string->list stra)]
	   [lsa (map (lambda (x)
		       (- (char->integer x)
			  (char->integer #\0)))
		     clsa)])
      lsa)))

(define sum-of-divisors
  (lambda (n)
    (cond
     ((= n 1) 0)
     (else
      (let loop ([i 2] [n n] [sum 1])
	(cond
	 ((and (<= (* i i) n) (> n 1))
	  (cond
	   ((zero? (remainder n i))
	    (let f ([p (* i i)] [num (/ n i)])
	      (cond
	       ((zero? (remainder num i))
		(f (* p i) (/ num i)))
	       (else
		(if (= i 2)
		    (loop 3 num (* sum (- p 1)))
		    (loop (+ i 2) num (/ (* sum (- p 1)) (- i 1))))))))
	   (else
	    (if (= i 2)
		(loop 3 n sum)
		(loop (+ i 2) n sum)))))
	 (else
	  (if (= n 1)
	      sum
	      (* (+ n 1) sum)))))))))

(define unique
  (lambda (ls)
    (cond
     ((null? ls) '())
     ((null? (cdr ls)) (cons (car ls) '()))
     ((member (car ls) (cdr ls))
      (unique (remove (car ls) ls)))
     (else
      (cons (car ls) (unique (cdr ls)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define exact-sqrt
  (lambda (n)
    (cond
     ((not (integer? n))
      (assertion-violation 'exact-sqrt "not an integer" n))
     ((negative? n)
      (assertion-violation 'exact-sqrt "not a non-negative integer" n))
     ((= n 0) 0)
     (else
      (let* ([num-of-digits (string-length (number->string n))]
	     [digits-required (quotient (+ num-of-digits 1) 2)]
	     [base-case (if (zero? (remainder num-of-digits 2)) 3 1)])
	(let f ([i (* base-case (expt 10 (- digits-required 1)))])
	  (let ([now (* i i)]
		[next (* (+ i 1) (+ i 1))])
	    (cond
	     ((= now n) i)
	     ((and (< now n) (> next n)) i)
	     (else (f (+ i 1)))))))))))

(define sqrt-con-frac
  (lambda (n)
    (cond
     ((not (integer? n))
      (assertion-violation 'sqrt-con-frac "not an integer" n))
     ((negative? n)
      (assertion-violation 'sqrt-con-frac "not a non-negative integer" n))
     (else
      (let ([fst (exact-sqrt n)])
	(letrec
	    ((A (lambda (numer denomi)
		  (let* ([new-numer (- n (* denomi denomi))]
			 [curr-numer (/ new-numer numer)]
			 [up (+ denomi fst)]
			 [beside (quotient up curr-numer)]
			 [remain (- fst (remainder up curr-numer))])
		    (if (= (* 2 fst) beside)
			(list beside)
			(cons beside (A curr-numer remain)))))))
	  (cond
	   ((= (* fst fst) n) (list fst))
	   (else
	    (cons fst (A 1 fst))))))))))

(define con-frac
  (lambda (conls)
    (cond
     ((not (list? conls))
      (assertion-violation 'con-frac "not a list" conls))
     ((null? conls)
      (assertion-violation 'con-frac "null list" conls))
     (else
      (let f ([ls (reverse conls)] [sum 0])
	(cond
	 ((null? (cdr ls)) (+ (car ls) sum))
	 (else (f (cdr ls) (/ (+ sum (car ls)))))))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define sublist
  (lambda (ls start end)
    (cond
     ((not (list? ls))
      (assertion-violation 'sublist "not a list" ls))
     ((null? ls) '())
     (else
      (let ([len (length ls)])
	(cond
	 ((> end len)
	  (assertion-violation 'sublist
			       "end index exceed!" (list '> end len)))
	 ((> start end)
	  (assertion-violation 'sublist
			       "start index is bigger than end index" (list '> start end)))
	 ((= start end) '())
	 (else
	  (let ([new-ls (list-tail ls start)]
		[end (- end start)])
	    (let f ([i 0] [ls new-ls])
	      (cond
	       ((= i end) '())
	       (else
		(cons (car ls)
		      (f (+ i 1) (cdr ls))))))))))))))

(define-syntax pop
  (syntax-rules()
    [(_ ls)
     (let ([len (length ls)])
       (cond
	((null? ls) '())
	((= len 1) (let ([now (car ls)]) (set! ls '()) now))
	(else
	 (let* ([now-ls (list-tail ls (- len 1))]
		[prev-ls (list-tail ls (- len 2))]
		[compond (car now-ls)])
	   (set-cdr! prev-ls '())
	   compond))))]))

(define get-eq-repeat
  (lambda (n)
    (let* ([num-ls (get-num n)]
	   [num-vec (list->vector num-ls)]
	   [num-len (vector-length num-vec)])
      (let f ([i 0] [ls '()])
	(cond
	 ((= i num-len) (reverse ls))
	 ((< (vector-ref num-vec i) 0)
	  (f (+ i 1) ls))
	 (else
	  (let ([now-num (vector-ref num-vec i)])
	    (let g ([j (+ i 1)] [now-ls (list i)])
	      (cond
	       ((>= j num-len) (f (+ i 1) (cons (reverse now-ls) ls)))
	       ((< (vector-ref num-vec j) 0)
		(g (+ j 1) now-ls))
	       ((= (vector-ref num-vec j) now-num)
		(vector-set! num-vec j -1)
		(g (+ j 1) (cons j now-ls)))
	       (else
		(g (+ j 1) now-ls)))))))))))

(define replace-hint
  (lambda (num ls i)
    (let ([num-str (number->string num)]
	  [char-i (integer->char (+ i (char->integer #\0)))])
      (let f ([now-ls ls])
	(cond
	 ((null? now-ls) (string->number num-str))
	 (else
	  (string-set! num-str (car now-ls) char-i)
	  (f (cdr now-ls))))))))


(define n!
  (lambda (n)
    (cond
     ((< n 0) #f)
     ((= n 0) 1)
     ((= n 1) 1)
     (else
      (* n (n! (- n 1)))))))

(define n!
  (lambda (n)
    (cond
     ((< n 0) #f)
     ((= n 0) 1)
     ((= n 1) 1)
     (else
      (let f ([n n] [pow 1])
	(cond
	 ((= n 1) pow)
	 (else
	  (f (- n 1) (* pow n)))))))))
(define Cn
  (lambda (m n)
    (/ (n! m) (n! (- m n)) (n! n))))

(define eular-filter
  (lambda (n)
    (let-syntax ([v-ref (syntax-rules ()
			  [(_ vec i)
			   (vector-ref vec (- i 1))])]
		 [v-set! (syntax-rules ()
			   [(_ vec i j)
			    (vector-set! vec (- i 1) j)])])
      (let ([vec (make-vector n 1)]
	    [half (quotient n 2)]
	    [prime-ls (list 2)])
	(letrec ([add-tail! (let ([ptr prime-ls])
			     (lambda (new)
			       (set-cdr! ptr (list new))
			       (set! ptr (cdr ptr))))])
	  (v-set! vec 1 0)
	  (v-set! vec 4 0)
	  (let f ([i 3])
	    (cond
	     [(> i half) (let f ([j i])
			   (cond
			    [(> j n) (list->vector prime-ls)]
			    [(not (zero? (v-ref vec j)))
			     (add-tail! j)
			     (f (+ j 2))]
			    [else (f (+ j 2))]))]
	     [else
	      (let g ([ls prime-ls])
		(cond
		 [(null? ls)
		  (when (not (zero? (v-ref vec i)))
		    (add-tail! i)
		    (if (<= (* i i) n) (v-set! vec (* i i) 0)))
		  (f (+ i 1))]
		 [else
		  (let ([now (car ls)])
		    (cond
		     [(> (* now i) n)
		      (if (not (zero? (v-ref vec i))) (add-tail! i))
		      (f (+ i 1))]
		     [(zero? (remainder i now))
		      (v-set! vec (* i now) 0)
		      (if (not (zero? (v-ref vec i))) (add-tail! i))
		      (f (+ i 1))]
		     [else
		      (v-set! vec (* now i) 0)
		      (g (cdr ls))]))]))])))))))

(define powermod
  (lambda (n k m)
    (let f ([n n] [k k] [r 1])
      (cond
       ((= k 0) r)
       ((= (remainder k 2) 1)
	(f n (- k 1) (remainder (* r n) m)))
       (else
	(f (remainder (* n n) m) (/ k 2) r))))))

