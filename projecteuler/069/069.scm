(define N 1000000)
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

(let f ([i 2] [max (cons 2 (/ 2 (vector-ref phi 2)))])
  (cond
   ((= i N) max)
   ((> (/ i (vector-ref phi i)) (cdr max))
    (f (+ i 1) (cons i (/ i (vector-ref phi i)))))
   (else
    (f (+ i 1) max))))

#|
(time (begin (define N ...) ...))
    20 collections
    0.406250000s elapsed cpu time, including 0.015625000s collecting
    0.397716552s elapsed real time, including 0.013619700s collecting
    85520592 bytes allocated, including 96862328 bytes reclaimed
(510510 . 17017/3072)
|#
