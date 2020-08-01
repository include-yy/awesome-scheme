(define N 10000000)

(begin
(define range N)
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

(define num-of-pri
  (let f ([i 1] [sum 1])
    (cond
     ((= i range) sum)
     ((prime-r? i) (f (+ i 1) (+ sum 1)))
     (else
      (f (+ i 1) sum)))))

(define prime-vec
  (make-vector num-of-pri 0))

(let takein ([i 1] [index 0])
  (cond
   ((= i range))
   ((prime-r? i)
    (vector-set! prime-vec index i)
    (takein (+ i 1) (+ index 1)))
   (else
    (takein (+ i 1) index))))

(define get-num
  (lambda (a)
    (let* ([stra (number->string a)]
	   [clsa (string->list stra)]
	   [lsa (map (lambda (x)
		       (- (char->integer x)
			  (char->integer #\0)))
		     clsa)])
      lsa)))

(define isperm
  (lambda (a b)
    (let ([a1 (sort < (get-num a))]
	  [b1 (sort < (get-num b))])
      (equal? a1 b1))))

(let pro ([a 0] [now (cons 2 2)])
  (cond
   ((< N (* (vector-ref prime-vec a)
	    (vector-ref prime-vec a))) now)
   (else
    (let f ([b (+ a 1)] [curr now])
      (let ([n (* (vector-ref prime-vec a)
		  (vector-ref prime-vec b))]
	    [c (* (- (vector-ref prime-vec a) 1)
		  (- (vector-ref prime-vec b) 1))])
	(cond
	 ((> n N) (pro (+ a 1) curr))
	 ((isperm n c)
	  (let ([r (/ n c)])
	    (if (> (cdr curr) r)
		(f (+ b 1) (cons n r))
		(f (+ b 1) curr))))
	 (else
	  (f (+ b 1) curr))))))))

#|
(time (let pro ...))
    629 collections
    2.000000000s elapsed cpu time, including 0.015625000s collecting
    2.004197132s elapsed real time, including 0.025961124s collecting
    2648556664 bytes allocated, including 2648226464 bytes reclaimed
(8319823 . 8319823/8313928)
|#
