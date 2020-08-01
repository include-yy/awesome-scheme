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
(define sod
  (lambda (n) (- (sum-of-divisors n) n)))

(define alist
  (let f ([i 12] [ls '()])
    (cond
     ((> i 30000) (reverse ls))
     (else
      (if (< i (sod i))
	  (f (+ i 1) (cons i ls))
	  (f (+ i 1) ls))))))
;;(length alist) => 7428
(define avec (list->vector alist))

(define find-vec (make-vector 30000 0))

(let f ([i 0])
  (cond
   ((> i 7427))
   (else
    (let g ([j i])
      (cond
       ((> j 7427) (f (+ i 1)))
       (else
	(let ([a (vector-ref avec i)]
	      [b (vector-ref avec j)])
	  (cond
	   ((< (+ a b) 30000)
	    (vector-set! find-vec (+ a b) 1)
	    (g (+ j 1)))
	   (else
	    (g (+ j 1)))))))))))
       
(let pro ([i 1] [sum 0])
  (cond
   ((> i 28123) sum)
   ((= (vector-ref find-vec i) 1)
    (pro (+ i 1) sum))
   (else
    (pro (+ i 1) (+ sum i)))))
