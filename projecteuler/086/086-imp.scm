(let pro ([k 1] [accu 0])
  (cond
   ((> accu 1000000) (- k 1))
   (else
    (let ([top (* k 2)] [now-exp (expt k 2)])
      (let f ([it 1] [acc-now 0])
	(cond
	 ((>= it top) (pro (+ k 1) (+ accu acc-now)))
	 ((integer? (sqrt (+ (expt it 2) (expt k 2))))
	  (f (+ it 1) (+ acc-now (if (< it k)
				     (quotient it 2)
				     (+ (quotient (- top it) 2) 1)))))
	 (else
	  (f (+ it 1) acc-now))))))))

#|
(time (let pro ...))
    43 collections
    0.828125000s elapsed cpu time, including 0.000000000s collecting
    0.823463900s elapsed real time, including 0.001751300s collecting
    183849064 bytes allocated, including 181117208 bytes reclaimed
1818
|#
