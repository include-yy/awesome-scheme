(define mat (make-vector 80 #f))

(define mat-ref
  (lambda (m i j)
    (vector-ref (vector-ref m i) j)))
(define mat-set!
  (lambda (m i j ne)
    (vector-set! (vector-ref m i) j ne)))

(call-with-input-file "p081_matrix.txt"
  (lambda (po)
    (let f ([i 0])
      (cond
       ((= i 80))
       (else
	(vector-set! mat i (make-vector 80 0))
	(let ([now (vector-ref mat i)])
	  (let g ([j 0][dat (get-datum po)])
	    (cond
	     ((= j 79)
	      (vector-set! now j dat)
	      (f (+ i 1)))
	     (else
	      (get-char po)
	      (vector-set! now j dat)
	      (g (+ j 1) (get-datum po)))))))))))

(let pro1 ([i 1])
  (cond
   ((= i 80))
   (else
    (mat-set! mat i 0 (+ (mat-ref mat i 0)
			 (mat-ref mat (- i 1) 0)))
    (mat-set! mat 0 i (+ (mat-ref mat 0 i)
			 (mat-ref mat 0 (- i 1))))
    (pro1 (+ i 1)))))

(let pro2 ([i 1])
  (cond
   ((= i 80) (mat-ref mat 79 79))
   (else
    (let f ([j 1])
      (cond
       ((= j 80) (pro2 (+ i 1)))
       (else
	(if (> (mat-ref mat i (- j 1))
	       (mat-ref mat (- i 1) j))
	    (mat-set! mat i j (+ (mat-ref mat i j)
				 (mat-ref mat (- i 1) j)))
	    (mat-set! mat i j (+ (mat-ref mat i j)
				 (mat-ref mat i (- j 1)))))
	(f (+ j 1))))))))

#|
(time (begin (call-with-input-file "p081_matrix.txt" ...) ...))
    no collections
    0.000000000s elapsed cpu time
    0.001085100s elapsed real time
    758968 bytes allocated
427337
|#
