(define mat-ref
  (lambda (m i j)
    (vector-ref (vector-ref m i) j)))
(define mat-set!
  (lambda (m i j ne)
    (vector-set! (vector-ref m i) j ne)))

(define N 80)
(define mat (make-vector N #f))
(call-with-input-file "p082_matrix.txt"
  (lambda (po)
    (let f ([i 0])
      (cond
       ((= i N))
       (else
	(vector-set! mat i (make-vector N 0))
	(let ([now (vector-ref mat i)])
	  (let g ([j 0][dat (get-datum po)])
	    (cond
	     ((= j (- N 1))
	      (vector-set! now j dat)
	      (f (+ i 1)))
	     (else
	      (get-char po)
	      (vector-set! now j dat)
	      (g (+ j 1) (get-datum po)))))))))))

(let pro ([j 1])
  (cond
   ((= j N) (do ([i 0 (+ i 1)]
		 [mi (mat-ref mat 0 (- N 1))
		     (if (< (mat-ref mat i (- N 1)) mi)
			 (mat-ref mat i (- N 1))
			 mi)])
	       ((= i N) mi)))
   (else
    (let f ([i 0])
      (cond
       ((= i N) (pro (+ j 1)))
       (else
	(let g ([k i] [mi (if (= i 0) (mat-ref mat i (- j 1)) (mat-ref mat (- i 1) j))])
	  (cond
	   ((= k N)
	    (mat-set! mat i j (+ mi (mat-ref mat i j)))
	    (f (+ i 1)))
	   ((= k i)
	    (if (< (mat-ref mat k (- j 1)) mi)
		(g (+ k 1) (mat-ref mat k (- j 1)))
		(g (+ k 1) mi)))
	   (else
	    (let ([now (+ (mat-ref mat k (- j 1))
			  (do ([n k (- n 1)] [sum 0 (+ sum (mat-ref mat n j))])
			      ((= n i) sum)))])
	      (if (< now mi)
		  (g (+ k 1) now)
		  (g (+ k 1) mi))))))))))))

#|
(time (let pro ...))
    no collections
    0.062500000s elapsed cpu time
    0.051665900s elapsed real time
    0 bytes allocated
260324
|#
