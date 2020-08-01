(define prime (make-bytevector 110000000 1))
(bytevector-u8-set! prime 1 0)

(let f ([i 2])
  (cond
   ((>= i 110000000))
   ((= (bytevector-u8-ref prime i) 1)
    (let g ([j (+ i i)])
      (cond
       ((>= j 110000000) (f (+ i 1)))
       (else
	(bytevector-u8-set! prime j 0)
	(g (+ j i))))))
   (else
    (f (+ i 1)))))

(define prime?
  (lambda (n)
    (cond
     ((or (= n 0) (= n 1)) #f)
     ((= (bytevector-u8-ref prime n) 1) #t)
     (else
      #f))))

(define divisor
  (lambda (n)
    (let f ([i 1])
      (cond
       ((> (* i i) n) '())
       ((= (remainder n i) 0)
	(cons i (f (+ i 1))))
       (else
	(f (+ i 1)))))))

(define satis?
  (lambda (n)
    (let ([ls (divisor n)])
      (let f ([ls ls])
	(cond
	 ((null? ls) #t)
	 (else
	  (let ([fst (car ls)])
	    (cond
	     ((prime? (+ fst (/ n fst)))
	      (f (cdr ls)))
	     (else #f)))))))))

(do ((i 2 (+ i 4)) (sum 1))
    ((> i 100000000)  sum)
  (if (satis? i) (set! sum (+ i sum))))

#|
(time (do ((...) (...)) ...))
    505 collections
    937.359375000s elapsed cpu time, including 0.203125000s collecting
    945.239266900s elapsed real time, including 0.219738800s collecting
    2127605408 bytes allocated, including 2236526600 bytes reclaimed
1739023853137
|#

#|
694444388888890
  
100      0.000013 1
1000     0.00021  16
10000    0.0039   18
100000   0.0684   17.5
1000000  1.9300   28.2
10000000 59.45    30
|#
