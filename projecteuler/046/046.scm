(define range 100000)
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

(define prime-list
  (let f ([i 1])
    (cond
     ((= i 100000) '())
     ((prime-r? i)
      (cons i (f (+ i 1))))
     (else
      (f (+ i 1))))))
(define exp2-list
  (let f ([i 0])
    (cond
     ((= i 1000) '())
     (else
      (cons (* i i) (f (+ i 1)))))))
(define satis?
  (lambda (n)
    (let f ([prime (cdr prime-list)])
      (cond
       ((> (car prime) n) #f)
       ((member (/ (- n (car prime)) 2) exp2-list)
	#t)
       (else
	(f (cdr prime)))))))

(let pro ([i 33])
  (cond
   ((satis? i)
    (pro (+ i 2)))
   (else i)))

#|
(time (let pro ...))
    no collections
    0.203125000s elapsed cpu time
    0.210631380s elapsed real time
    0 bytes allocated
5777
|#
