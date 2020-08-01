(define T-vec (make-vector 21 0))
(do ([i 0 (+ i 1)])
    ((= i 21))
  (vector-set! T-vec i (make-vector 11 0)))
(define-syntax m-r
  (syntax-rules ()
    [(_ m i j)
     (vector-ref (vector-ref m i) j)]))
(define-syntax m-s!
  (syntax-rules ()
    [(_ m i j n)
     (vector-set! (vector-ref m i) j n)]))
(m-s! T-vec 1 1 1)

(let pro ([n 2])
  (cond
   ((> n 20))
   (else
    (let f ([k 1])
      (cond
       ((> k 10) (pro (+ n 1)))
       (else
	(m-s! T-vec n k (+ (* (m-r T-vec (- n 1) k)
			      (/ (- (* 10.0 k) (- n 1))
				 (- 70 (- n 1))))
			   (* (m-r T-vec (- n 1) (- k 1))
			      (/ (- 70 (* 10.0 (- k 1)))
				 (- 70 (- n 1))))))
	(f (+ k 1))))))))

(do ([k 1 (+ k 1)] [sum 0 (+ sum (* (m-r  T-vec 20 k) k))])
    ((> k 7) sum))

#|
(time (begin (let pro ...) ...))
    no collections
    0.000000000s elapsed cpu time
    0.000017300s elapsed real time
    21056 bytes allocated
6.818741802019762
|#
