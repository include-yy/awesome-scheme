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

(define deal
  (lambda (n)
    (let ([vec (make-vector (+ n 1) 0)])
      (vector-set! vec 0 1)
      (vector-set! vec 1 1)
      (let f ([i 1] [sum 1])
	(cond
	 ((= i n) sum)
	 (else
	  (do ([k (+ i 1) (- k 1)])
	      ((= k 0))
	    (vector-set! vec k
			 (+ (vector-ref vec k)
			    (vector-ref vec (- k 1)))))
	  (let ([now (let g ([j 1] [pow 1])
		       (cond
			((= j (+ i 1)) pow)
			(else
			 (g (+ j 1) (* (vector-ref vec j) pow)))))])
	    (f (+ i 1) (remainder (+ sum (sum-of-divisors now)) 1000000007)))))))))
;;very slow
