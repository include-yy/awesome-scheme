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

(define formu1
  (lambda (n)
    (+ (* n n) (- n) 1)))

(define formu2
  (lambda (N)
    (+ (* 4 N N) 1)))

(define total
  (lambda (N)
    (let ([cur (+ (* N 2) 1)])
      (- (* cur 2) 1))))

(let pro ([N 1] [pri-num 3] [tt (total 1)])
  (cond
   ((< (/ pri-num tt) 1/10) (+ (* N 2) 1))
   (else
    (let f ([i (+ (* N 2) 2)] [sum 0])
      (cond
       ((<= i (+ (* N 2) 3))
	(if (prime? (formu1 i))
	    (f (+ i 1) (+ sum 1))
	    (f (+ i 1) sum)))
       (else
	  (if (prime? (formu2 (+ N 1)))
	      (pro (+ N 1)
		   (+ pri-num sum 1)
		   (+ tt 4))
	      (pro (+ N 1)
		   (+ pri-num sum)
		   (+ tt 4)))))))))

#|
(time (let pro ...))
    1 collection
    0.234375000s elapsed cpu time, including 0.000000000s collecting
    0.233416128s elapsed real time, including 0.000070152s collecting
    4740152 bytes allocated, including 4190016 bytes reclaimed
26241
|#
