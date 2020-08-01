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

(define ini-sieve
  (let f ([b 0] [ls '()])
    (cond
     ((> b 1000) ls)
     ((prime? b)
      (let g ([a -999] [las ls])
	(cond
	 ((> a 999) (f (+ b 1) las))
	 ((and (prime? (+ a b 1)) (prime? (+ 4 (* 2 a) b)) (prime? (+ 9 (* 3 a) b)) (prime? (+ 16 (* 4 a) b)))
	  (g (+ a 1)(cons (cons a b) las)))
	 (else
	  (g (+ a 1) las)))))
     (else
      (f (+ b 1) ls)))))

(let pro ([ls ini-sieve] [max '(0 0 0)])
  (cond
   ((null? ls) max)
   (else
    (let f ([i 0] [a (car (car ls))] [b (cdr (car ls))])
      (cond
       ((prime? (+ (* i i) (* i a) b))
	(f (+ i 1) a b))
       (else
	(if (> i (list-ref max 2))
	    (pro (cdr ls) (list a b i))
	    (pro (cdr ls) max))))))))
#|
(time (let pro ...))
    1 collection
    0.015625000s elapsed cpu time, including 0.000000000s collecting
    0.014139180s elapsed real time, including 0.000070152s collecting
    2380744 bytes allocated, including 4192112 bytes reclaimed
(-61 971 71)
|#
