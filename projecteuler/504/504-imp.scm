(define squ-vec (make-vector 50000 0))

(let p ([i 1])
  (cond
   ((>= (* i i) 50000) #t)
   (else
    (vector-set! squ-vec (* i i) 1)
    (p (+ i 1)))))

(define square?
  (lambda (m)
    (not (zero? (vector-ref squ-vec m)))))

(define line-p
  (lambda (x y)
    (- (gcd x y) 1)))

(define find-inner-p
  (lambda (a b c d)
    (let ([l-p (+ (line-p a b)
		  (line-p c b)
		  (line-p a d)
		  (line-p c d))])
      (/ (+ (* (+ a c) (+ b d)) (- (+ l-p 4)) 2) 2))))

(define find-pt
  (lambda (m)
    (let ([now 0])
      (let f1 ([i1 1])
	(cond
	 ((> i1 m) now)
	 (else
	  (let f2 ([i2 1])
	    (cond
	     ((> i2 m) (f1 (+ i1 1)))
	     (else
	      (let f3 ([i3 1])
		(cond
		 ((> i3 m) (f2 (+ i2 1)))
		 (else
		  (let f4 ([i4 1])
		    (cond
		     ((> i4 m) (f3 (+ i3 1)))
		     (else
		      (let ([i-p (find-inner-p i1 i2 i3 i4)])
			(cond
			 ((square? i-p)
			  (set! now (+ now 1))
			  (f4 (+ i4 1)))
			 (else
			  (f4 (+ i4 1))))))))))))))))))))

(find-pt 100)
#|
(time (find-pt 100))
    380 collections
    18.187500000s elapsed cpu time, including 0.000000000s collecting
    18.185732400s elapsed real time, including 0.011586800s collecting
    1600135880 bytes allocated, including 1600270984 bytes reclaimed
694687
|#
