(define find
  (lambda (n)
    (+
     (let ([sum 0])
       (let f1 ([i1 1])
	 (cond
	  ((= i1 (+ n 1)) sum)
	  (else
	   (let f2 ([i2 (+ i1 1)])
	     (cond
	      ((= i2 (+ n 1)) (f1 (+ i1 1)))
	      (else
	       (let f3 ([i3 (+ i2 1)])
		 (cond
		  ((= i3 (+ n 1)) (f2 (+ i2 1)))
		  (else
		   (let ([one (sqrt (+ (expt (+ i1 i2) 2) (expt i3 2)))]
			 [two (sqrt (+ (expt (+ i2 i3) 2) (expt i1 2)))]
			 [thr (sqrt (+ (expt (+ i1 i3) 2) (expt i2 2)))])
		     (let ([mm (min one two thr)])
		       (when (integer? mm) (set! sum (+ sum 1)))
		       (f3 (+ i3 1))))))))))))))

     (let ([sum 0])
       (let f1 ([i1 1] [i2 1])
	 (cond
	  ((= i1 (+ n 1)) sum)
	  (else
	   (let f2 ([i3 (+ i1 1)])
	     (cond
	      ((= i3 (+ n 1)) (f1 (+ i1 1) (+ i2 1)))
	      (else
	       (let ([one (sqrt (+ (expt (+ i1 i2) 2) (expt i3 2)))]
		     [two (sqrt (+ (expt (+ i2 i3) 2) (expt i1 2)))]
		     [thr (sqrt (+ (expt (+ i1 i3) 2) (expt i2 2)))])
		 (let ([mm (min one two thr)])
		   (when (integer? mm) (set! sum (+ sum 1)))
		   (f2 (+ i3 1)))))))))))

     (let ([sum 0])
       (let f1 ([i1 1])
	 (cond
	  ((= i1 (+ n 1)) sum)
	  (else
	   (let f2 ([i2 (+ i1 1)] [i3 (+ i1 1)])
	     (cond
	      ((= i3 (+ n 1)) (f1 (+ i1 1)))
	      (else
	       (let ([one (sqrt (+ (expt (+ i1 i2) 2) (expt i3 2)))]
		     [two (sqrt (+ (expt (+ i2 i3) 2) (expt i1 2)))]
		     [thr (sqrt (+ (expt (+ i1 i3) 2) (expt i2 2)))])
		 (let ([mm (min one two thr)])
		   (when (integer? mm) (set! sum (+ sum 1)))
		   (f2 (+ i2 1) (+ i3 1))))))))))))))

;;slow and stupid

(let ([k 102])
  (do ([i1 1 (+ i1 1)] [cnt 0])
      ((> i1 k) cnt)
    (do ([i2 i1 (+ i2 1)])
	((> i2 k))
      (do ([i3 i2 (+ i3 1)])
	  ((> i3 k))
	(let ([now (sqrt (+ (expt (+ i1 i2) 2)
			    (expt i3 2)))])
	  (cond
	   ((not (integer? now)))
	   (else
	    (set! cnt (+ cnt 1)))))))))
