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

(begin
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
)

(define prime-n?
  (lambda (n)
    (if (>= n 1000000000)
	(prime? n)
	(prime-r? n))))

(define prime-vec (eular-filter 100000))

(define prime-len (vector-length prime-vec))

(define isok
  (lambda (a b)
    (let ([str1 (number->string a)]
	  [str2 (number->string b)])
      (if (prime? (string->number (string-append str1 str2)))
	  (prime? (string->number (string-append str2 str1)))
	  #f))))

(let f1 ([i1 1])
  (cond
   ((= i1 1228) #f)
   (else
    (let ([one (vector-ref prime-vec i1)])
      (let f2 ([i2 (+ i1 1)])
	(let ([two (vector-ref prime-vec i2)])
	  (cond
	   ((= i2 1229) (f1 (+ i1 1)))
	   ((not (isok one two))
	    (f2 (+ i2 1)))
	   (else
	    (let f3 ([i3 (+ i2 1)])
	      (let ([three (vector-ref prime-vec i3)])
		(cond
		 ((= i3 1230) (f2 (+ i2 1)))
		 ((or (not (isok one three))
		      (not (isok two three)))
		  (f3 (+ i3 1)))
		 (else
		  (let f4 ([i4 (+ i3 1)])
		    (let ([four (vector-ref prime-vec i4)])
		      (cond
		       ((= i4 1231) (f3 (+ i3 1)))
		       ((or (not (isok one four))
			    (not (isok two four))
			    (not (isok three four)))
			(f4 (+ i4 1)))
		       (else
			(let f5 ([i5 (+ i4 1)])
			  (let ([five (vector-ref prime-vec i5)])
			    (cond
			     ((= i5 1232) (f4 (+ i4 1)))
			     ((or (not (isok one five))
				  (not (isok two five))
				  (not (isok three five))
				  (not (isok four five)))
			      (f5 (+ i5 1)))
			     (else
			      (list one two three four five)))))))))))))))))))))

#|
(time (let f1 ...))
    193 collections
    1.156250000s elapsed cpu time, including 0.000000000s collecting
    1.160015600s elapsed real time, including 0.008579800s collecting
    812902880 bytes allocated, including 812787016 bytes reclaimed
(13 5197 5701 6733 8389)
|#
