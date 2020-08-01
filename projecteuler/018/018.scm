(define tri-arr
  (let ([vec (make-vector 15)])
    (let f ([i 0])
      (cond
       ((= i 15) vec)
       (else
	(vector-set! vec i (make-vector (+ i 1)))
	(f (+ i 1)))))))

(call-with-input-file "data.txt"
  (lambda (po)
    (let f ([i 0])
      (cond
       ((= i 15))
       (else
	(let g ([j 0])
	  (cond
	   ((> j i) (f (+ i 1)))
	   (else
	    (vector-set! (vector-ref tri-arr i) j (get-datum po))
	    (g (+ j 1))))))))))

(define tri-ref
  (lambda (tri m n)
    (vector-ref (vector-ref tri m) n)))
(define tri-set!
  (lambda (tri m n val)
    (vector-set! (vector-ref tri m) n val)))

(let f ([i 13])
  (cond
   ((= i 0)
    (let ([a (tri-ref tri-arr 1 0)]
	  [b (tri-ref tri-arr 1 1)])
      (if (> a b)
	  (+ (tri-ref tri-arr 0 0) a)
	  (+ (tri-ref tri-arr 0 0) b))))
   (else
    (let g ([j 0])
      (cond
       ((> j i) (f (- i 1)))
       (else
	(let ([a (tri-ref tri-arr (+ i 1) j)]
	      [b (tri-ref tri-arr (+ i 1) (+ j 1))]
	      [now (tri-ref tri-arr i j)])
	  (if (> a b)
	      (tri-set! tri-arr i j (+ now a))
	      (tri-set! tri-arr i j (+ now b)))
	  (g (+ j 1)))))))))
