(define tri-vec (make-vector 100))
(let f ([i 0])
  (cond
   ((= i 100))
   (else
    (vector-set! tri-vec i (make-vector (+ i 1) 0))
    (f (+ i 1)))))

(call-with-input-file "p067_triangle.txt"
  (lambda (po)
    (let f ([now (get-datum po)] [i 0] [j 0])
      (cond
       ((eof-object? now))
       (else
	(vector-set! (vector-ref tri-vec i) j now)
	(if (= i j)
	    (f (get-datum po) (+ i 1) 0)
	    (f (get-datum po) i (+ j 1))))))))

(define tri-ref
  (lambda (tri m n)
    (vector-ref (vector-ref tri m) n)))
(define tri-set!
  (lambda (tri m n val)
    (vector-set! (vector-ref tri m) n val)))

(let f ([i 98])
  (cond
   ((= i 0)
    (let ([a (tri-ref tri-vec 1 0)]
	  [b (tri-ref tri-vec 1 1)])
      (if (> a b)
	  (+ (tri-ref tri-vec 0 0) a)
	  (+ (tri-ref tri-vec 0 0) b))))
   (else
    (let g ([j 0])
      (cond
       ((> j i) (f (- i 1)))
       (else
	(let ([a (tri-ref tri-vec (+ i 1) j)]
	      [b (tri-ref tri-vec (+ i 1) (+ j 1))]
	      [now (tri-ref tri-vec i j)])
	  (if (> a b)
	      (tri-set! tri-vec i j (+ now a))
	      (tri-set! tri-vec i j (+ now b)))
	  (g (+ j 1)))))))))

#|
(time (let f ...))
    no collections
    0.000000000s elapsed cpu time
    0.000150960s elapsed real time
    0 bytes allocated
7273
|#
