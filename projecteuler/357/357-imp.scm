(define prime (make-bytevector 110000000 1))
(bytevector-u8-set! prime 1 0)

(let f ([i 2])
  (cond
   ((>= i 110000000))
   ((= (bytevector-u8-ref prime i) 1)
    (let g ([j (+ i i)])
      (cond
       ((>= j 110000000) (f (+ i 1)))
       (else
	(bytevector-u8-set! prime j 0)
	(g (+ j i))))))
   (else
    (f (+ i 1)))))

(define prime?
  (lambda (n)
    (cond
     ((or (= n 0) (= n 1)) #f)
     ((= (bytevector-u8-ref prime n) 1) #t)
     (else
      #f))))

(let pro ([n 2] [sum 1])
  (cond
   ((= n 100000000) sum)
   (else
    (cond
     ((prime? (+ n 1)) 
      (let g ([d 2])
	(cond
	 ((and (zero? (remainder n d)) (not (prime? (+ d (/ n d)))))
	  (pro (+ n 1) sum))
	 ((> (* d d) n) (pro (+ n 1) (+ sum n)))
	 (else (g (+ d 1))))))
     (else (pro (+ n 1) sum))))))

#|
(time (let pro ...))
    no collections
    2.328125000s elapsed cpu time
    2.374372400s elapsed real time
    599400 bytes allocated
1739023853137
|#
