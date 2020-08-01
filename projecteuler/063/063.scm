(let pro ([i 1] [sum 0])
  (cond
   ((= i 10) sum)
   (else
    (let f ([j 1] [curr 0])
      (cond
       ((>= (expt i j) (expt 10 (- j 1)))
	(f (+ j 1) (+ curr 1)))
       (else
	(pro (+ i 1) (+ sum curr))))))))

#|
(time (let pro ...))
    no collections
    0.000000000s elapsed cpu time
    0.000009768s elapsed real time
    728 bytes allocated
49
|#
