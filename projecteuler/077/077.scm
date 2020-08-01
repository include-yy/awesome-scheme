(begin
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

(define range 10000)
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

(define prime-len
  (let f ([i 3] [count 1])
    (cond
     ((= i range) count)
     ((prime-r? i) (f (+ i 1) (+ count 1)))
     (else
      (f (+ i 1) count)))))

(define prime-vec (make-vector prime-len 0))
(vector-set! prime-vec 0 2)

(let f ([i 3] [j 1])
  (cond
   ((= prime-len j))
   ((prime-r? i)
    (vector-set! prime-vec j i)
    (f (+ i 1) (+ j 1)))
   (else
    (f (+ i 1) j))))

(define pronow
  (lambda (amount)
    (define tovec (make-vector (+ amount 1) 0))
    (vector-set! tovec 0 1)
    (let pro ([i 0])
      (cond
       ((> (vector-ref prime-vec i) amount)
	(vector-ref tovec amount))
       (else
	(let f ([j (vector-ref prime-vec i)])
	  (cond
	   ((> j amount) (pro (+ i 1)))
	   (else
	    (vector-set! tovec j
			 (+ (vector-ref tovec j)
			    (vector-ref tovec (- j (vector-ref prime-vec i)))))
	    (f (+ j 1))))))))))

(let f ([i 10])
  (cond
   ((> (pronow i) 5000) i)
   (else
    (f (+ i 1)))))

#|
(time (let f ...))
    no collections
    0.015625000s elapsed cpu time
    0.000207792s elapsed real time
    10664 bytes allocated
71
|#
