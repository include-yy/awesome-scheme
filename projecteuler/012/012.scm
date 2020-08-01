(define tri-num
  (lambda (n)
    (/ (* n (+ n 1)) 2)))

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

(define prime-deposit
  (lambda (n)
    (let ([p2 0])
      (let f ([i n])
	(cond
	 ((= (remainder i 2) 0)
	  (set! p2 (+ p2 1))
	  (f (/ i 2)))
	 (else
	  (let g ([j 3] [n i] [lst (list (list 2 p2))])
	    (cond
	     ((= n 1) (reverse lst))
	     (else
	      (let k ([ass 0][now n])
		(cond
		 ((and (prime? j)
		       (= (remainder now j) 0))
		  (k (+ ass 1) (/ now j)))
		 (else
		  (g (+ j 2) now (cons (list j ass) lst))))))))))))))

(define sum-of-factor
  (lambda (n)
    (let ([lst (prime-deposit n)])
      (let f ([lst lst] [pow 1])
	(cond
	 ((null? lst) pow)
	 (else
	  (f (cdr lst) (* pow (+ 1 (cadar lst))))))))))

(time(let pro ([i 1])
  (cond
   ((> (sum-of-factor (tri-num i)) 500) (tri-num i))
   (else
    (pro (+ i 1))))))
#|(time (let pro ...))
    380 collections
    4.531250000s elapsed cpu time, including 0.046875000s collecting
    4.563060932s elapsed real time, including 0.019494708s collecting
    1601924680 bytes allocated, including 1600788784 bytes reclaimed
76576500
|#

(define imp-sum-of-factor
  (lambda (n)
    (let f ([num n] [numof2 1])
      (cond
       ((= (remainder num 2) 0)
	(f (/ num 2) (+ numof2 1)))
       (else
	(let g ([i 3] [pow numof2] [num1 num])
	  (cond
	   ((= num1 1) pow)
	   ((and (prime? i)
		 (= (remainder num1 i) 0))
	    (let h ([numofi 2] [num2 (/ num1 i)])
	      (cond
	       ((= (remainder num2 i) 0)
		(h (+ numofi 1) (/ num2 i)))
	       (else
		(g (+ i 2) (* pow numofi) num2)))))
	   (else
	    (g (+ i 2) pow num1)))))))))

(let pro ([i 1])
  (let ([tri (tri-num i)])
    (cond
     ((> (imp-sum-of-factor tri) 500) tri)
     (else
      (pro (+ i 1))))))
#|(time (let pro ...))
    278 collections
    4.281250000s elapsed cpu time, including 0.000000000s collecting
    4.284129364s elapsed real time, including 0.005354640s collecting
    1169562992 bytes allocated, including 1171252616 bytes reclaimed
76576500
|#
