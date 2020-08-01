(define range 10000)
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

(define prime-len
  (let f ([i 1] [count 0])
    (cond
     ((= i range) count)
     ((prime-r? i)
      (f (+ i 1) (+ count 1)))
     (else
      (f (+ i 1) count)))))

(define prime-vec
  (let ([vec (make-vector prime-len 0)])
    (let f ([i 1] [j 0])
      (cond
       ((= i range) vec)
       ((prime-r? i)
	(vector-set! vec j i)
	(f (+ i 1) (+ j 1)))
       (else
	(f (+ i 1) j))))))

(define F
  (lambda (m)
    (let ([q (quotient m 6)] [r (remainder m 6)])
      (let ([apart (if (= r 5) 1 0)])
	(+ (* q (+ (* q 3) (- 2) r )) apart)))))

(let pro ([limit range] [index 0] [count (F range)])
    (cond
     ((and (< index prime-len)
	   (<= (* 5 (vector-ref prime-vec index)) limit))
      (let ([newlimit (quotient limit (vector-ref prime-vec index))])
	    (pro limit (+ index 1)
		 (- count (pro newlimit (+ index 1) (F newlimit))))))
     (else
      count)))

#|
(time (let pro ...))
    no collections
    0.000000000s elapsed cpu time
    0.000057276s elapsed real time
    0 bytes allocated
5066251
|#
