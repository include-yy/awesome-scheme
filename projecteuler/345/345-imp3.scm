(define mat (make-vector 15 #f))

(call-with-input-file "data.txt"
  (lambda (po)
    (let f ([i 0])
      (cond
       ((= i 15) #t)
       (else
	(vector-set! mat i (make-vector 15 0))
	(let g ([j 0])
	  (cond
	   ((= j 15) (f (+ i 1)))
	   (else
	    (let ([now (get-datum po)])
	      (vector-set! (vector-ref mat i) j now)
	      (g (+ j 1)))))))))))

(define y (bitwise-arithmetic-shift-left 1 15))
(define x 15)

(define dp (make-vector x #f))

(do ([i 0 (+ i 1)]) ((= i x))
  (vector-set! dp i (make-vector y -1)))

(define-syntax m-r
  (syntax-rules ()
    [(_ m i j)
     (vector-ref (vector-ref m i) j)]))
(define-syntax m-s!
  (syntax-rules ()
    [(_ m i j ne)
     (vector-set! (vector-ref m i) j ne)]))

(define findit
  (lambda (row mask)
    (cond
     ((= row 15) 0)
     ((not (= (m-r dp row mask) -1))
      (m-r dp row mask))
     (else
      (let ([retval
	     (let f ([j 0] [curr-max 0])
	       (cond
		((= j 15) curr-max)
		((= (bitwise-and 1 (bitwise-arithmetic-shift-right mask j)) 1)
		 (f (+ j 1)
		    (max curr-max (+ (m-r mat row j)
				     (findit (+ row 1)
					     (bitwise-xor
					      mask
					      (bitwise-arithmetic-shift-left 1 j)))))))
		(else
		 (f (+ j 1) curr-max))))])
	(m-s! dp row mask retval)
	retval)))))

(findit 0 (- (bitwise-arithmetic-shift-left 1 15) 1))

#|
(time (findit 0 ...))
    no collections
    0.000000000s elapsed cpu time
    0.009356300s elapsed real time
    0 bytes allocated
13938
|#
