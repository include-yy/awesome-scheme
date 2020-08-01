(define N 1000001)
(define phi (make-vector N 0))
(vector-set! phi 1 1)

(let pro ([i 2])
  (cond
   ((= i N))
   ((zero? (vector-ref phi i))
    (let f ([j i])
      (cond
       ((>= j N) (pro (+ i 1)))
       ((zero? (vector-ref phi j))
	(vector-set! phi j j)
	(vector-set! phi j (* (- i 1) (vector-ref phi j) (/ i)))
	(f (+ j i)))
       (else
	(vector-set! phi j (* (- i 1) (vector-ref phi j) (/ i)))
	(f (+ j i))))))
   (else
    (pro (+ i 1)))))

(do ([i 1 (+ i 1)] [sum 0 (+ sum (vector-ref phi i))]) ((> i 1000000) sum))

#|
(time (begin (define N ...) ...))
    18 collections
    0.312500000s elapsed cpu time, including 0.000000000s collecting
    0.310303720s elapsed real time, including 0.009847920s collecting
    78050896 bytes allocated, including 73163424 bytes reclaimed
303963552392
|#
