(define get-num
  (lambda (a)
    (let* ([stra (number->string a)]
	   [clsa (string->list stra)]
	   [lsa (map (lambda (x)
		       (- (char->integer x)
			  (char->integer #\0)))
		     clsa)])
      lsa)))

(define satis?
  (lambda (i)
    (let ([ist (sort < (get-num i))])
      (let f ([now 2])
	(cond
	 ((= now 7) #t)
	 (else
	  (and (equal? ist (sort < (get-num (* now i))))
	       (f (+ now 1)))))))))

(let pro ([max 10])
  (let ([limit (quotient max 6)])
    (let f ([i (/ max 10)])
      (cond
       ((> i limit) (pro (* max 10)))
       ((satis? i) i)
       (else
	(f (+ i 1)))))))

#|
(time (let pro ...))
    16 collections
    0.046875000s elapsed cpu time, including 0.000000000s collecting
    0.046664400s elapsed real time, including 0.000323232s collecting
    65472584 bytes allocated, including 67346008 bytes reclaimed
142857
|#
