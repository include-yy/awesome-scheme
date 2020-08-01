(define s 1000)

(define s2 (/ s 2))

(define mlimit (inexact->exact (- (ceiling (sqrt s2)) 1)))

(let pro ([m 2])
  (cond
   ([> m mlimit])
   ([= (remainder s2 m) 0]
    (let ([sm (/ s2 m)])
      (let f ()
	(when (= (remainder sm 2) 0)
	  (set! sm (/ sm 2))
	  (f)))
      (let g ([k (if (= (remainder m 2) 1)
		   (+ m 2)
		   (+ m 1))])
	(if (and (< k (* 2 m)) (<= k sm))
	    (cond
	     ((and (= (remainder sm k) 0)
		   (= (gcd k m) 1))
	      (let* ([d (/ s2 k m)]
		     [n (- k m)]
		     [a (* d (- (* m m) (* n n )))]
		     [b (* 2 d m n)]
		     [c (* d (+ (* m m) (* n n)))])
		(values a b c)))
	     (else
	      (g (+ k 2))))
	    (pro (+ m 1))))))
   (else
    (pro (+ m 1)))))
	    
	  
   
