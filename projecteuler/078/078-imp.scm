(begin
(define limit 1000000)
(define p-vec (make-vector (+ limit 1) 0))
(vector-set! p-vec 0 1)
(vector-set! p-vec 1 1)
)

(let pro ([n 1])
  (let f ([i 0])
    (let ([i1 (+ i 1)]
	  [n1 (+ n 1)])
      (let ([m1 (- n1 (quotient (* i1 (- (* i1 3) 1)) 2))]
	    [m2 (- n1 (quotient (* i1 (+ (* i1 3) 1)) 2))]
	    [s (if (zero? (remainder i1 2)) -1 1)])
	(cond
	 ((and (< m1 0) (< m2 0))
	  (vector-set! p-vec n1
		       (remainder (vector-ref p-vec n1) 1000000))
	  (cond
	   ((zero? (vector-ref p-vec n1)) n1)
	   (else (pro n1))))
	 (else
	  (if (>= m1 0) (vector-set! p-vec n1
				     (+ (* s (vector-ref p-vec m1))
					(vector-ref p-vec n1))))
	  (if (>= m2 0) (vector-set! p-vec n1
				     (+ (* s (vector-ref p-vec m2))
					(vector-ref p-vec n1))))
				     
	  (f i1)))))))

#|
(time (let pro ...))
    no collections
    0.203125000s elapsed cpu time
    0.211758364s elapsed real time
    0 bytes allocated
55374
|#
