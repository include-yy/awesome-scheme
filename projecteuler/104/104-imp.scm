(define 1-to-9
  (let ([lst '(1 2 3 4 5 6 7 8 9)])
    (lambda (str)
      (let ([me (sort < (map (lambda (x)
			     (- (char->integer x)
				(char->integer #\0)))
			   (string->list str)))])
	(equal? lst me)))))

(define one-billion 1000000000)

(define find-under-m
  (lambda (m)
    (let f ([k1 3] [a1 1] [b1 2]
	    [k2 2] [a2 1] [b2 1])
      (cond
       ((or (> k1 m) (> k2 m))
	(values m k1 k2))
       ((= k1 k2) k1)
       ((> k1 k2)
	(let f2 ([k (+ k2 1)] [a b2] [b (+ a2 b2)])
	  (cond
	   ((> k m) (values m k1 k2))
	   (else
	    (let* ([now (number->string b)]
		   [len (string-length now)])
	      (cond
	       [(< len 9)
		(f2 (+ k 1) b (+ a b))]
	       [(= len 9)
		(if (1-to-9 now)
		    (f k1 a1 b1
		       k a b)
		    (f2 (+ k 1) b (+ a b)))]
	       [else
		(let ([lst (substring now (- len 9) len)])
		  (if (1-to-9 lst)
		      (f k1 a1 b1
			 k (remainder a one-billion) (remainder b one-billion))
		      (f2 (+ k 1) (remainder b one-billion)
			  (remainder (+ a b) one-billion))))]))))))
       ((< k1 k2)
	(let f1 ([k (+ k1 1)] [a b1] [b (+ a1 b1)])
	  (cond
	   ((> k m) (values m k1 k2))
	   (else
	    (let* ([now (number->string b)]
		   [len (string-length now)])
	      (cond
	       [(< len 30)
		(f1 (+ k 1) b (+ a b))]
	       [(= len 30)
		(let ([lst (substring now 0 9)])
		  (if (1-to-9 lst)
		      (f k a b
			 k2 a2 b2)
		      (f1 (+ k 1) b (+ a b))))]
	       [else
		(let ([lst (substring now 0 9)])
		  (if (1-to-9 lst)
		      (f k a b
			 k2 a2 b2)
		      (f1 (+ k 1) (quotient b 10)
			  (quotient (+ a b) 10))))]))))))))))

(find-under-m 1000000)

#|
(time (find-under-m 1000000))
    182 collections
    0.796875000s elapsed cpu time, including 0.000000000s collecting
    0.801479000s elapsed real time, including 0.003167300s collecting
    766915072 bytes allocated, including 766548128 bytes reclaimed
329468
|#
