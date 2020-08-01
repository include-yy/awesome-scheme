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
(define find-point
  (lambda (x0 y0)
    (let ([k (/ y0 x0)])
      (let f ([det-x (+ (- x0) 1)] [tot 0])
	(cond
	 ((= det-x 0) tot)
	 (else
	  (let ([now (ceiling (+ (* (/ y0 x0) det-x) y0 -1))])
	    (f (+ det-x 1) (+ tot now)))))))))

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
		      (let ([on-line (+ i1 i2 i3 i4 -3)])
			(let ([points (+ on-line
					 (find-point i2 i1)
					 (find-point i2 i3)
					 (find-point i4 i1)
					 (find-point i4 i3))])
			  (cond
			   ((square? points)
			    (set! now (+ now 1))
			    (f4 (+ i4 1)))
			   (else
			    (f4 (+ i4 1)))))))))))))))))))))

;;too slow
