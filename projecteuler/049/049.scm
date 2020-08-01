(define range 15000)
(define sievebound (quotient (- range 1) 2))
(define crosslimit (quotient (- (exact (floor (sqrt range))) 1) 2))

(define prime (make-bytevector sievebound 1))
(define prime-ref
  (lambda (pme i)
    (bytevector-u8-ref pme (- i 1))))
(define prime-set!
  (lambda (pme i arg)
    (bytevector-u8-set! pme (- i 1) arg)))
    
(let filt ([i 1])
  (cond
   ((> i sievebound))
   ((= (prime-ref prime i) 1)
    (let f ([j (* 2 i (+ i 1))])
      (cond
       ((> j sievebound) (filt (+ i 1)))
       (else
	(prime-set! prime j 0)
	(f (+ j (+ (* i 2) 1)))))))
   (else
    (filt (+ i 1)))))

(define prime-r?
  (lambda (n)
    (cond
     ((<= n 0) #f)
     ((= n 1) #f)
     ((= n 2) #t)
     ((= (remainder n 2) 0) #f)
     ((= (prime-ref prime (/ (- n 1) 2)) 1) #t)
     (else #f))))
(define get-num
  (lambda (a)
    (let* ([stra (number->string a)]
	   [clsa (string->list stra)]
	   [lsa (map (lambda (x)
		       (- (char->integer x)
			  (char->integer #\0)))
		     clsa)])
      lsa)))

(let pro ([i 1001])
  (cond
   ((> i 10000) '())
   ((prime-r? i)
    (let ([num (sort < (get-num i))])
      (let f ([j 1])
	(cond
	 ((>= (+ i (* j 2)) 10000) (pro (+ i 2)))
	 ((and (prime-r? (+ i j))
	       (prime-r? (+ i (* 2 j)))
	       (equal? (sort < (get-num (+ i j))) num)
	       (equal? (sort < (get-num (+ i (* j 2)))) num))
	  (cons (list i (+ i j) (+ i (* j 2)))
		(f (+ j 1))))
	 (else
	  (f (+ j 1)))))))
   (else (pro (+ i 2)))))

#|
(time (let pro ...))
    6 collections
    0.093750000s elapsed cpu time, including 0.000000000s collecting
    0.087019116s elapsed real time, including 0.000201132s collecting
    25477112 bytes allocated, including 25217712 bytes reclaimed
((1487 4817 8147) (2969 6299 9629))
|#
