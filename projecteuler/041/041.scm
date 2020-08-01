;;1000000000
(define get-num
  (lambda (a)
    (let* ([stra (number->string a)]
	   [clsa (string->list stra)]
	   [lsa (map (lambda (x)
		       (- (char->integer x)
			  (char->integer #\0)))
		     clsa)])
      lsa)))
(define range 100000000)
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
     ((= (remainder n 2) 0) #f)
     ((= (prime-ref prime (/ (- n 1) 2)) 1) #t)
     (else #f))))

(car(reverse 
(let f ([i 101])
  (let ([ls (sort < (get-num i))])
    (cond
     ((> i 100000000) '())
     ((prime-r? i)
      (cond
       ((or (equal? ls '(1 2 3 4 5 6 7 8 9))
	    (equal? ls '(1 2 3 4 5 6 7 8))
	    (equal? ls '(1 2 3 4 5 6 7))
	    (equal? ls '(1 2 3 4 5 6))
	    (equal? ls '(1 2 3 4 5))
	    (equal? ls '(1 2 3 4))
	    (equal? ls '(1 2 3)))
	(cons i (f (+ i 2))))
       (else
	(f (+ i 2)))))
     (else (f (+ i 2))))))))

#|
(time (car (reverse (...))))
    8750 collections
    31.468750000s elapsed cpu time, including 1.140625000s collecting
    32.009164604s elapsed real time, including 1.197147100s collecting
    36855610464 bytes allocated, including 36856944256 bytes reclaimed
7652413
|#
