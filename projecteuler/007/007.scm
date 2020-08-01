(define prime (make-bytevector 1000000 1))

(bytevector-u8-set! prime 1 0)

(let filt ([i 2])
  (cond
   ((>= i 1000000))
   ((= (bytevector-u8-ref prime i) 1)
    (let f ([j (+ i i)])
      (cond
       ((>= j 1000000) (filt (+ i 1)))
       (else
	(bytevector-u8-set! prime j 0)
	(f (+ j i))))))
   (else
    (filt (+ i 1)))))
#|
(time (let filt ...))
    no collections
    0.031250000s elapsed cpu time
    0.016660656s elapsed real time
    0 bytes allocated
|#

(time (let pro ([i 2] [index 0])
  (cond
   ((>= i 1000000) #f)
   ((= index 10001) (- i 1))
   ((= (bytevector-u8-ref prime i) 1)
    (pro (+ i 1) (+ index 1)))
   (else
    (pro (+ i 1) index)))))

#|
(time (let pro ...))
    no collections
    0.000000000s elapsed cpu time
    0.000567432s elapsed real time
    0 bytes allocated
104743
|#
