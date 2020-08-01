(define range 2000000)
(define half-limit (inexact->exact (floor (sqrt range))))

 
(define prime (make-bytevector (+ 1 range) 1))

(bytevector-u8-set! prime 0 0)
(bytevector-u8-set! prime 1 0)

(let filt1 ([i 4])
  (cond
   ((> i range))
   (else
    (bytevector-u8-set! prime i 0)
    (filt1 (+ i 2)))))

(let filt2 ([i 3])
  (cond
   ((> i half-limit))
   ((= (bytevector-u8-ref prime i) 1)
    (let f ([j (* i i)])
      (cond
       ((> j range) (filt2 (+ i 2)))
       (else
	(bytevector-u8-set! prime j 0)
	(f (+ j (* i 2)))))))
   (else
    (filt2 (+ i 2)))))

(let f ([i 2] [sum 0])
  (cond
   ((> i range) sum)
   ((= (bytevector-u8-ref prime i) 1)
    (f (+ i 1) (+ sum i)))
   (else
    (f (+ i 1) sum))))
