(let pro ([i 2] [ls '()])
  (cond
   ((> i 100) (length ls))
   (else
    (let f ([j 2] [ls ls])
      (cond
       ((> j 100) (pro (+ i 1) ls))
       (else
	(let ([now (expt i j)])
	  (if (member now ls)
	      (f (+ j 1) ls)
	      (f (+ j 1) (cons now ls))))))))))
#|
(time (let pro ...))
    1 collection
    0.703125000s elapsed cpu time, including 0.000000000s collecting
    0.704478928s elapsed real time, including 0.000396492s collecting
    1341872 bytes allocated, including 3808584 bytes reclaimed
9183
|#
      
