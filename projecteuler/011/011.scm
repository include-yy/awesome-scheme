(define make-matrix
  (lambda (m n)
    (let ([vec1 (make-vector m)])
      (let f ([i 0])
	(cond
	 ((>= i m) vec1)
	 (else
	  (vector-set! vec1 i (make-vector n))
	  (f (+ i 1))))))))
(define matrix-ref
  (lambda (mat i j)
    (let ([veci (vector-ref mat (- i 1))])
      (vector-ref veci (- j 1)))))
(define matrix-set!
  (lambda (mat i j val)
    (let ([veci (vector-ref mat (- i 1))])
      (vector-set! veci (- j 1) val))))

(define now-mat (make-matrix 20 20))

(define data (open-file-input-port "data.txt" (file-options no-create) (buffer-mode block) (native-transcoder)))

(call-with-port
 data
 (lambda (po)
   (let f ([num (get-datum po)] [i 1] [j 1])
     (cond
      ((eof-object? num))
      ((= j 20)
       (matrix-set! now-mat i j num)
       (f (get-datum po) (+ i 1) 1))
      (else
       (matrix-set! now-mat i j num)
       (f (get-datum po) i (+ j 1)))))))

(define left-right
  (let f ([i 1] [max 0])
    (cond
     ((> i 20) max)
     (else
      (let g ([j 1] [curr 0])
	(cond
	 ((> j 17)
	  (if (> curr max)
	      (f (+ i 1) curr)
	      (f (+ i 1) max)))
	 (else
	  (let ([cur (do ((k j (+ k 1)) (pow 1 (* pow (matrix-ref now-mat i k)))) ((= k (+ j 4)) pow))])
	    (if (> cur curr)
		(g (+ j 1) cur)
		(g (+ j 1) curr))))))))))
(define up-down
  (let f ([j 1] [max 0])
    (cond
     ((> j 20) max)
     (else
      (let g ([i 1] [curr 0])
	(cond
	 ((> i 17)
	  (if (> curr max)
	      (f (+ j 1) curr)
	      (f (+ j 1) max)))
	 (else
	  (let ([cur (do ((k i (+ k 1)) (pow 1 (* pow (matrix-ref now-mat k j)))) ((= k (+ i 4)) pow))])
	    (if (> cur curr)
		(g (+ i 1) cur)
		(g (+ i 1) curr))))))))))
(define leftup-rightdown
  (let f ([i 1] [max 0])
    (cond
     ((> i 17) max)
     (else
      (let g ([j 1] [curr 0])
	(cond
	 ((> j 17)
	  (if (> curr max)
	      (f (+ i 1) curr)
	      (f (+ i 1) max)))
	 (else
	  (let ([cur (do ((p i (+ p 1)) (q j (+ q 1)) (pow 1 (* pow (matrix-ref now-mat p q)))) ((= p (+ i 4)) pow))])
	    (if (> cur curr)
		(g (+ j 1) cur)
		(g (+ j 1) curr))))))))))

(define leftdown-rightup
  (let f ([i 1] [max 0])
    (cond
     ((> i 17) max)
     (else
      (let g ([j 20] [curr 0])
	(cond
	 ((< j 4)
	  (if (> curr max)
	      (f (+ i 1) curr)
	      (f (+ i 1) max)))
	 (else
	  (let ([cur (do ((p i (+ p 1)) (q j (- q 1)) (pow 1 (* pow (matrix-ref now-mat p q)))) ((= p (+ i 4)) pow))])
	    (if (> cur curr)
		(g (- j 1) cur)
		(g (- j 1) curr))))))))))
