(define N 12000)

(let f ([i 1] [sum 0])
  (cond
   ((= i N) sum)
   (else
    (let g ([j (+ i 1)] [now 0])
      (let ([ne (/ i j)])
	(cond
	 ((> j N) (f (+ i 1) (+ sum now)))
	 ((= i (numerator ne))
	  (if (and (< ne 1/2) (> ne 1/3))
	      (g (+ j 1) (+ now 1))
	      (g (+ j 1) now)))
	 (else
	  (g (+ j 1) now))))))))

#|
(time (let f ...))
    272 collections
    7.296875000s elapsed cpu time, including 0.015625000s collecting
    7.342427896s elapsed real time, including 0.008888436s collecting
    1152194784 bytes allocated, including 1149035200 bytes reclaimed
7295372
|#
