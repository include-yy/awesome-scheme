(define range 1100000)
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

(define div10?
  (lambda (n)
    (cond
     ((< n 10) #f)
     ((= n 0) #t)
     ((zero? (remainder n 10)) #t)
     (else
      (div10? (quotient n 10))))))

(define digits-of-digit
  (lambda (n)
    (string-length (number->string n))))

(define rotate
  (lambda (n)
    (let ([last (remainder n 10)]
	  [remain (quotient n 10)]
	  [digs (digits-of-digit n)])
      (+ (* last (expt 10 (- digs 1))) remain))))

(define rotate-prime?
  (lambda (n)
    (if (div10? n) #f
	(let ([digs (digits-of-digit n)])
	  (let f ([now n] [i 0])
	    (cond
	     ((= i digs) #t)
	     ((prime-r? now)
	      (f (rotate now) (+ i 1)))
	     (else #f)))))))

(let pro ([i 2][sum 0])
  (cond
   ((= i 1000000) sum)
   ((rotate-prime? i)
    (pro (+ i 1) (+ 1 sum)))
   (else
    (pro (+ i 1) sum))))

#|
(time (let pro ...))
    71 collections
    0.265625000s elapsed cpu time, including 0.000000000s collecting
    0.266829792s elapsed real time, including 0.002103228s collecting
    301742992 bytes allocated, including 299191864 bytes reclaimed
55
|#
