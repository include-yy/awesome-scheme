(define prime (make-bytevector 2000000 1))

(bytevector-u8-set! prime 1 0)

(let filt ([i 2])
  (cond
   ((>= i 2000000))
   ((= (bytevector-u8-ref prime i) 1)
    (let f ([j (+ i i)])
      (cond
       ((>= j 2000000) (filt (+ i 1)))
       (else
	(bytevector-u8-set! prime j 0)
	(f (+ j i))))))
   (else
    (filt (+ i 1)))))
#|
(time (let filt ...))
    no collections
    0.031250000s elapsed cpu time
    0.031859664s elapsed real time
    0 bytes allocated
|#

(let pro ([i 2] [sum 0])
  (cond
   ((>= i 2000000) sum)
   ((= (bytevector-u8-ref prime i) 1)
    (pro (+ i 1) (+ sum i)))
   (else
    (pro (+ i 1) sum))))

#|
(time (let pro ...))
    no collections
    0.015625000s elapsed cpu time
    0.009503820s elapsed real time
    2076896 bytes allocated
142913828922
|#
