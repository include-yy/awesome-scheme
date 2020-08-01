(define prime?
  (lambda (n)
    (cond
     ((<= n 0) #f)
     ((= n 1) #f)
     ((< n 4) #t)
     ((= (remainder n 2) 0) #f)
     ((< n 9) #t)
     ((= (remainder n 3) 0) #f)
     (else
      (let ([r (inexact->exact (floor (sqrt n)))])
	(let f ([i 5])
	  (cond
	   ((> i r) #t)
	   ((= (remainder n i) 0) #f)
	   ((= (remainder n (+ i 2)) 0) #f)
	   (else
	    (f (+ i 6))))))))))
