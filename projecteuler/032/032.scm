;;5 + 4
;;5 = 1 + 4 = 2 + 3

(define get-num
  (lambda (a)
    (let* ([stra (number->string a)]
	   [clsa (string->list stra)]
	   [lsa (map (lambda (x)
		       (- (char->integer x)
			  (char->integer #\0)))
		     clsa)])
      lsa)))

(define satisfied?
  (lambda (a b c)
    (equal? (sort < (append (get-num a)
			    (get-num b)
			    (get-num c)))
	    '(1 2 3 4 5 6 7 8 9))))

(define divisors
  (lambda (n)
    (let f ([i 1])
      (cond
       ((> (* i i) n) '())
       (else
	(cond
	 ((zero? (remainder n i))
	  (cons (list i (/ n i)) (f (+ i 1))))
	 (else
	  (f (+ i 1)))))))))

(apply + (let pro ([i 1234][sls '()])
	   (cond
	    ((= i 10000) sls)
	    (else
	     (let ([nls (divisors i)])
	       (let g ([ls nls])
		 (cond
		  ((null? ls) (pro (+ i 1) sls))
		  ((apply satisfied? i (car ls) )
		   (pro (+ i 1) (cons i sls)))
		  (else
		   (g (cdr ls))))))))))
#|
(time (apply + ...))
    18 collections
    0.046875000s elapsed cpu time, including 0.000000000s collecting
    0.054463704s elapsed real time, including 0.000314352s collecting
    75247752 bytes allocated, including 75794248 bytes reclaimed
45228
|#
