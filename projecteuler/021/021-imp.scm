(define sum-of-divisors
  (lambda (n)
    (cond
     ((= n 1) 0)
     (else
      (let loop ([i 2] [n n] [sum 1])
	(cond
	 ((and (<= (* i i) n) (> n 1))
	  (cond
	   ((zero? (remainder n i))
	    (let f ([p (* i i)] [num (/ n i)])
	      (cond
	       ((zero? (remainder num i))
		(f (* p i) (/ num i)))
	       (else
		(if (= i 2)
		    (loop 3 num (* sum (- p 1)))
		    (loop (+ i 2) num (/ (* sum (- p 1)) (- i 1))))))))
	   (else
	    (if (= i 2)
		(loop 3 n sum)
		(loop (+ i 2) n sum)))))
	 (else
	  (if (= n 1)
	      sum
	      (* (+ n 1) sum)))))))))

	      

(let por ([a 2][sum 0])
  (let ([b (- (sum-of-divisors a) a)])
    (if (> a 10000)
	sum
	(if (> b a)
	    (if (= (- (sum-of-divisors b) b) a)
		(por (+ a 1) (+ sum a b))
		(por (+ a 1) sum))
	    (por (+ a 1) sum)))))

#|
(time (let por ...))
    no collections
    0.000000000s elapsed cpu time
    0.004171824s elapsed real time
    0 bytes allocated
31626
|#
