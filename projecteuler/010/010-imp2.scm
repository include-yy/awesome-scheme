(define range 2000000)
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

(let pro ([i 1] [sum 2])
  (cond
   ((> i sievebound) sum)
   ((= (prime-ref prime i) 1)
    (pro (+ i 1) (+ sum (+ (* 2 i) 1))))
   (else
    (pro (+ i 1) sum))))
