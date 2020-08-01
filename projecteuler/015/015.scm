(define Latpa
  (lambda (m n)
    (cond
     ((or (zero? m) (zero? n))
      1)
     (else
      (+ (Latpa (- m 1) n) (Latpa m (- n 1)))))))

;;-----------------------------------------------

(define memory
  (let ([cache '()])
    (lambda (proc)
      (lambda (x)
	(cond
	 [(assq x cache) => cdr]
	 [else
	  (let ([ans (proc x)])
	    (set! cache (cons (cons x ans) cache))
	    ans)])))))
(define memory-grid
  (lambda (proc)
    (let ([cache '()])
      (lambda (m n)
	(cond
	 [(assoc (cons m n) cache) => cdr]
	 [else
	  (let ([ans (proc m n)])
	    (set! cache (cons (cons (cons m n) ans) cache))
	    ans)])))))
(define Latpa-2
  (memory-grid
   (lambda (m n)
     (cond
      ((or (= m 1) (= n 1))
       1)
      (else
       (+ (Latpa-2 (- m 1) n) (Latpa-2 m (- n 1))))))))

;;-----------------------------------------------------

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

(define grid 21)
(define matgrid (make-matrix grid grid))
(do ((i 1 (+ i 1))) ((> i grid))
  (matrix-set! matgrid 1 i 1))
(do ((j 1 (+ j 1))) ((> j grid))
  (matrix-set! matgrid j 1 1))

(let pro ([i 2])
  (cond
   ((> i grid)(matrix-ref matgrid grid grid))
   (else
    (let f ([j 2])
      (cond
       ((> j grid) (pro (+ i 1)))
       (else
	(matrix-set! matgrid i j
		     (+ (matrix-ref matgrid (- i 1) j)
			(matrix-ref matgrid i (- j 1))))
	(f (+ j 1))))))))

;;------------------------------------------------------

