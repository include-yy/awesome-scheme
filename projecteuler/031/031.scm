(define coins (list 1 2 5 10 20 50 100 200))
(define fin2 200)

(let pro ([sum fin2] [lscoin coins])
  (cond
   ((< sum 0) 0)
   ((zero? sum) 1)
   ((null? lscoin) 0)
   (else
    (+ (pro (- sum (car lscoin)) lscoin)
       (pro sum (cdr lscoin))))))

#|
(time (let pro ...))
    no collections
    0.062500000s elapsed cpu time
    0.060562600s elapsed real time
    0 bytes allocated
73682
|#

(let pro2 ([amount fin2] [cins (reverse coins)])
  (cond
   ((null? (cdr cins)) 1)
   (else
    (let f ([amt amount] [cin cins])
      (cond
       ((< amt 0) 0)
       ((= amt 0) 1)
       (else
	(+ (f (- amt (car cin)) cin)
	   (pro2 amt (cdr cin)))))))))

#|
(time (let pro2 ...))
    no collections
    0.000000000s elapsed cpu time
    0.000564324s elapsed real time
    64 bytes allocated
73682
|#

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

(define coins-vec (vector 1 2 5 10 20 50 100 200))
(define array-ref (lambda (vec i) (vector-ref vec (- i 1))))

(define coin-mat (make-matrix 200 8))

(let pro3 ([amount fin2] [index 8])
  (cond
   ((<= index 1) 1)
   (else
    (let f ([amt amount] [cin index])
      (cond
       ((< amt 0) 0)
       ((= amt 0) 1)
       (else
	(cond
	 ((not (zero? (matrix-ref coin-mat amt index)))
	  (matrix-ref coin-mat amt index))
	 (else
	  (let ([res (+ (f (- amt (array-ref coins-vec index)) cin)
			(pro3 amt (- cin 1)))])
	    (matrix-set! coin-mat amt cin res)
	    res)))))))))

#|
(time (let pro3 ...))
    no collections
    0.000000000s elapsed cpu time
    0.000010212s elapsed real time
    0 bytes allocated
73682
|#

(define coins-vec (vector 1 2 5 10 20 50 100 200))
(define array-ref (lambda (vec i) (vector-ref vec (- i 1))))
(define fin2 200)
(define ways (make-vector 201 0))

(vector-set! ways 0 1)

(let pro5 ([i 1])
  (cond
   ((> i 8) (vector-ref ways fin2))
   (else
    (let f ([j (array-ref coins-vec i)])
      (cond
       ((> j 200) (pro5 (+ i 1)))
       (else
	(vector-set! ways j
		     (+ (vector-ref ways j)
			(vector-ref ways (- j (array-ref coins-vec i)))))
	(f (+ j 1))))))))

#|
(time (let pro5 ...))
    no collections
    0.000000000s elapsed cpu time
    0.000012876s elapsed real time
    0 bytes allocated
73682
|#
