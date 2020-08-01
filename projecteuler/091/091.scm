(let f1 ([x1 0] [sum 0])
  (cond
   ((> x1 50) (/ sum 2))
   (else
    (let f2 ([y1 0] [sum1 sum])
      (cond
       ((> y1 50) (f1 (+ x1 1) sum1))
       ((and (= x1 0) (= y1 0))
	(f2 (+ y1 1) sum1))
       (else
	(let f3 ([x2 0] [sum2 sum1])
	  (cond
	   ((> x2 50) (f2 (+ y1 1) sum2))
	   (else
	    (let f4 ([y2 0] [sum3 sum2])
	      (cond
	       ((> y2 50) (f3 (+ x2 1) sum3))
	       ((or (and (= x2 0) (= y2 0))
		    (and (= x1 x2) (= y1 y2))
		    (zero? (- (* x1 y2) (* y1 x2))))
		(f4 (+ y2 1) sum3))
	       ((or (zero? (+ (* x1 x2) (* y1 y2)))
		    (zero? (+ (* x1 (- x1 x2))
			      (* y1 (- y1 y2))))
		    (zero? (+ (* x2 (- x1 x2))
			      (* y2 (- y1 y2)))))
		(f4 (+ y2 1) (+ sum3 1)))
	       (else (f4 (+ y2 1) sum3)))))))))))))

#|
(time (let f1 ...))
    no collections
    0.093750000s elapsed cpu time
    0.092294600s elapsed real time
    0 bytes allocated
14234
|#
