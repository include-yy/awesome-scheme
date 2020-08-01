(begin
(define make-matrix
  (lambda (m n)
    (let ([vec1 (make-vector m)])
      (let f ([i 0])
	(cond
	 ((>= i m) vec1)
	 (else
	  (vector-set! vec1 i (make-vector n 0))
	  (f (+ i 1))))))))

(define matrix-ref
  (lambda (mat i j)
    (let ([veci (vector-ref mat (- i 1))])
      (vector-ref veci (- j 1)))))

(define matrix-set!
  (lambda (mat i j val)
    (let ([veci (vector-ref mat (- i 1))])
      (vector-set! veci (- j 1) val))))
)

(define tower (make-matrix 100 100))

(matrix-set! tower 1 1 1)

(let pro ([i 2])
  (cond
   ((> i 100) (matrix-ref tower 100 1))
   (else
    (matrix-set! tower i i 1)
    (let f ([j (- i 1)])
      (cond
       ((= j 0) (pro (+ i 1)))
       (else
	(let ([now (- i j)])
	  (if (< now j)
	      (matrix-set! tower i j (matrix-ref tower i (+ j 1)))
	      (matrix-set! tower i j (+ (matrix-ref tower i (+ j 1))
					(matrix-ref tower (- i j) j))))
	  (f (- j 1)))))))))

#|
(time (let pro ...))
    no collections
    0.000000000s elapsed cpu time
    0.000197136s elapsed real time
    0 bytes allocated
190569292
|#


