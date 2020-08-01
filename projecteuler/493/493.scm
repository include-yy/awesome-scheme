(define fac
  (lambda (n)
    (cond
     ((< n 0) 0.0)
     ((= n 0) 1.0)
     ((= n 1) 1.0)
     (else
      (* n (fac (- n 1)))))))

(define Com
  (lambda (m n)
    (cond
     ((or (< m 0) (< n 0)) 0)
     ((< m n) 0)
     ((= n 0) 1)
     (else
      (/ (fac m) (* (fac (- m n))(fac n)))))))

(define gong (Com 70 20))

(define ratio-gong
  (lambda (n)
    (/ (Com 7 n) gong)))

(define r2
  (let ([tot 0.0])
    (let f1 ([i1 1])
      (cond
       ((> i1 10))
       (else
	(let f2 ([i2 1])
	  (cond
	   ((> i2 10) (f1 (+ i1 1)))
	   ((= (+ i1 i2) 20)
	    (set! tot (+ tot (* (Com 10 i1)
				(Com 10 i2))))
	    (f1 (+ i1 1)))
	   (else
	    (f2 (+ i2 1))))))))
    (* (ratio-gong 2) tot)))

(define r3
  (let ([tot 0.0])
    (let f1 ([i1 1])
      (cond
       ((> i1 10))
       (else
	(let f2 ([i2 1])
	  (cond
	   ((> i2 10) (f1 (+ i1 1)))
	   (else
	    (let f3 ([i3 1])
	      (cond
	       ((> i3 10) (f2 (+ i2 1)))
	       ((= (+ i1 i2 i3) 20)
		(set! tot (+ tot (* (Com 10 i1)
				    (Com 10 i2)
				    (Com 10 i3))))
		(f2 (+ i2 1)))
	       (else
		(f3 (+ i3 1)))))))))))
    (* (ratio-gong 3) tot)))

(define r4
  (let ([tot 0.0])
    (let f1 ([i1 1])
      (cond
       ((> i1 10))
       (else
	(let f2 ([i2 1])
	  (cond
	   ((> i2 10) (f1 (+ i1 1)))
	   (else
	    (let f3 ([i3 1])
	      (cond
	       ((> i3 10) (f2 (+ i2 1)))
	       (else
		(let f4 ([i4 1])
		  (cond
		   ((> i4 10) (f3 (+ i3 1)))
		   ((= (+ i1 i2 i3 i4) 20)
		    (set! tot (+ tot (* (Com 10 i1)
					(Com 10 i2)
					(Com 10 i3)
					(Com 10 i4))))
		    (f3 (+ i3 1)))
		   (else
		    (f4 (+ i4 1))))))))))))))
    (* (ratio-gong 4) tot)))

(define r5
  (let ([tot 0.0])
    (let f1 ([i1 1])
      (cond
       ((> i1 10))
       (else
	(let f2 ([i2 1])
	  (cond
	   ((> i2 10) (f1 (+ i1 1)))
	   (else
	    (let f3 ([i3 1])
	      (cond
	       ((> i3 10) (f2 (+ i2 1)))
	       (else
		(let f4 ([i4 1])
		  (cond
		   ((> i4 10) (f3 (+ i3 1)))
		   (else
		    (let f5 ([i5 1])
		      (cond
		       ((> i5 10) (f4 (+ i4 1)))
		       ((= (+ i1 i2 i3 i4 i5) 20)
			(set! tot (+ tot (* (Com 10 i1)
					    (Com 10 i2)
					    (Com 10 i3)
					    (Com 10 i4)
					    (Com 10 i5))))
			(f4 (+ i4 1)))
		       (else
			(f5 (+ i5 1)))))))))))))))))
    (* (ratio-gong 5) tot)))

(define r6
  (let ([tot 0.0])
    (let f1 ([i1 1])
      (cond
       ((> i1 10))
       (else
	(let f2 ([i2 1])
	  (cond
	   ((> i2 10) (f1 (+ i1 1)))
	   (else
	    (let f3 ([i3 1])
	      (cond
	       ((> i3 10) (f2 (+ i2 1)))
	       (else
		(let f4 ([i4 1])
		  (cond
		   ((> i4 10) (f3 (+ i3 1)))
		   (else
		    (let f5 ([i5 1])
		      (cond
		       ((> i5 10) (f4 (+ i4 1)))
		       (else
			(let f6 ([i6 1])
			  (cond
			   ((> i6 10) (f5 (+ i5 1)))
			   ((= (+ i1 i2 i3 i4 i5 i6) 20)
			    (set! tot (+ tot (* (Com 10 i1)
						(Com 10 i2)
						(Com 10 i3)
						(Com 10 i4)
						(Com 10 i5)
						(Com 10 i6))))
			    (f5 (+ i5 1)))
			   (else
			    (f6 (+ i6 1))))))))))))))))))))
    (* (ratio-gong 6) tot)))

(define r7
  (let ([tot 0.0])
    (let f1 ([i1 1])
      (cond
       ((> i1 10))
       (else
	(let f2 ([i2 1])
	  (cond
	   ((> i2 10) (f1 (+ i1 1)))
	   (else
	    (let f3 ([i3 1])
	      (cond
	       ((> i3 10) (f2 (+ i2 1)))
	       (else
		(let f4 ([i4 1])
		  (cond
		   ((> i4 10) (f3 (+ i3 1)))
		   (else
		    (let f5 ([i5 1])
		      (cond
		       ((> i5 10) (f4 (+ i4 1)))
		       (else
			(let f6 ([i6 1])
			  (cond
			   ((> i6 10) (f5 (+ i5 1)))
			   (else
			    (let f7 ([i7 1])
			      (cond
			       ((> i7 10) (f6 (+ i6 1)))
			       ((= (+ i1 i2 i3 i4 i5 i6 i7) 20)
				(set! tot (+ tot (* (Com 10 i1)
						    (Com 10 i2)
						    (Com 10 i3)
						    (Com 10 i4)
						    (Com 10 i5)
						    (Com 10 i6)
						    (Com 10 i7))))
			       (f6 (+ i6 1)))
			       (else
			       (f7 (+ i7 1)))))))))))))))))))))))
    (* (ratio-gong 7) tot)))


(+ (* 2 r2)
   (* 3 r3)
   (* 4 r4)
   (* 5 r5)
   (* 6 r6)
   (* 7 r7))
;;6.8187418020197175
;;ugly

