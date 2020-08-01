(define one-million 1000000)

(define c-vector
  (make-vector 101))

(let deal1 ([i 1])
  (cond
   ((> i 101))
   (else
    (vector-set! c-vector (- i 1) (make-vector i 0))
    (vector-set! (vector-ref c-vector (- i 1)) 0 1)
    (vector-set! (vector-ref c-vector (- i 1)) (- i 1) 1)
    (deal1 (+ i 1)))))

(let deal2 ([i 2])
  (cond
   ((> i 100))
   (else
    (let ([now (vector-ref c-vector i)]
	  [pre (vector-ref c-vector (- i 1))])
    (let f ([j 1])
      (cond
       ((= j i) (deal2 (+ i 1)))
       (else
	(let ([val (+ (vector-ref pre j)  (vector-ref pre (- j 1)))])
	  (if (> val one-million)
	      (vector-set! now j (+ 1 one-million))
	      (vector-set! now j val))
	  (f (+ j 1))))))))))

(define C
  (lambda (m n)
    (vector-ref (vector-ref c-vector m) n)))
	  
(let pro ([n 10] [sum 0])
  (cond
   ((> n 100) sum)
   (else
    (if (<= (C n (quotient n 2)) one-million)
	(pro (+ n 1) sum)
	(let f ([i 1])
	  (cond
	   ((<= (C n i) one-million) (f (+ i 1)))
	   (else
	    (pro (+ n 1) (+ sum (- n (* i 2) -1))))))))))

;;another approach
(define max 100)
(define limit 1000000)
(define c2-vector (make-vector (+ max 1) 1))

(let pro ([n 2] [count 0])
  (cond
   ((> n max) count)
   (else
    (vector-set! c2-vector (- n 1) 1)
    (let f ([r (- n 1)])
      (cond
       ((= r 0) (pro (+ n 1) count))
       (else
	(vector-set! c2-vector r
		     (+ (vector-ref c2-vector r)
			(vector-ref c2-vector (- r 1))))
	(if (> (vector-ref c2-vector r) limit)
	    (begin (vector-set! c2-vector r limit)
		   (pro (+ n 1) (+ count (+ r r 1 (- n)))))
	    (f (- r 1)))))))))

#|
(time (let pro ...))
    no collections
    0.000000000s elapsed cpu time
    0.000006660s elapsed real time
    1248 bytes allocated
4075
|#
