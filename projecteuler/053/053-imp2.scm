(define max 100)
(define limit 1000000)

(let pro ([n max] [nCr 1] [count 0] [r 0])
  (let f ([re r] [curr nCr])
    (cond
     ((>= re (/ n 2)) count)
     (else
      (let ([Cr (* curr (- n re) (/ (+ re 1)))])
	(if (<= Cr limit)
	    (f (+ re 1) Cr)
	    (pro (- n 1)
		 (* curr (- n re) (/ n))
		 (- (+ count n) (+ (* re 2) 1))
		 re)))))))

#|
(time (let pro ...))
    no collections
    0.000000000s elapsed cpu time
    0.000022200s elapsed real time
    3344 bytes allocated
4075
|#
