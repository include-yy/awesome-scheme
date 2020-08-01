(define prime-fac
  (lambda (n)
    (letrec
	((A (lambda (n q)
	      (if (zero? (remainder n q))
		  (A (/ n q) q)
		  n))))
      (let f ([n n][i 2][sum 0])
	(cond
	 ((= n 1) sum)
	 ((zero? (remainder n i))
	  (f (A n i) (+ i 1) (+ sum 1)))
	 (else
	  (f n (+ i 1) sum)))))))

(let pro ([i 100000])
  (if (= 4 (prime-fac i))
      (if (= 4 (prime-fac (+ i 1)))
	  (if (= 4 (prime-fac (+ i 2)))
	      (if (= 4 (prime-fac (+ i 3)))
		  i
		  (pro (+ i 4)))
	      (pro (+ i 3)))
	  (pro (+ i 2)))
      (pro (+ i 1))))

#|
(time (let pro ...))
    no collections
    3.187500000s elapsed cpu time
    3.226145184s elapsed real time
    0 bytes allocated
134043
|#
