;;first
(define limit 1000000)

(define phis1 (make-vector (+ limit 1) 0))

(do ([i 0 (+ i 1)]) ((> i limit) #t)
  (vector-set! phis1 i i))

(let pro ([n 2])
  (cond
   ((> n limit))
   (else
    (cond
     ((= n (vector-ref phis1 n))
      (let f ([m n])
	(cond
	 ((> m limit) (pro (+ n 1)))
	 (else
	  (let ([phism (vector-ref phis1 m)])
	    (vector-set! phis1 m
			 (- phism (/ phism n)))
	    (f (+ m n)))))))
     (else
      (pro (+ n 1)))))))

#|
(time (let pro ...))
    no collections
    0.093750000s elapsed cpu time
    0.089638716s elapsed real time
    0 bytes allocated
#t
|#
(do ([i 2 (+ i 1)] [count 0 (+ count (vector-ref phis1 i))]) ((> i limit) count))


;;second
(define limit 1000000)
(define phis2 (make-vector (+ limit 1) 0))
(define sieveLimit
  (let-values ([(a b) (exact-integer-sqrt limit)]) a))
(define spf (make-vector (+ limit 1) 0))

(do ([i 2 (+ i 1)]) ((> i limit) #t)
  (if (zero? (remainder i 2))
      (vector-set! spf i 2)
      (vector-set! spf i i)))

(let pro1 ([n 3])
  (cond
   ((> n sieveLimit))
   ((= (vector-ref spf n) n)
    (let f ([m (* n n)])
      (cond
       ((> m limit) (pro1 (+ n 2)))
       ((= (vector-ref spf m) m)
	(vector-set! spf m n)
	(f (+ m (* n 2))))
       (else
	(f (+ m (* n 2)))))))
   (else
    (pro1 (+ n 2)))))

(let pro2 ([n 2])
  (cond
   ((> n limit))
   ((= (vector-ref spf n) n)
    (vector-set! phis2 n (- n 1))
    (pro2 (+ n 1)))
   (else
    (let* ([p (vector-ref spf n)]
	   [m (/ n p)])
      (if (= (vector-ref spf m) p)
	  (vector-set! phis2 n (* p (vector-ref phis2 m)))
	  (vector-set! phis2 n (* (- p 1) (vector-ref phis2 m))))
      (pro2 (+ n 1))))))

(do ([i 2 (+ i 1)] [count 0 (+ count (vector-ref phis2 i))]) ((> i limit) count))

#|
(time (begin (do (...) ...) ...))
    4 collections
    0.078125000s elapsed cpu time, including 0.000000000s collecting
    0.077814664s elapsed real time, including 0.006264840s collecting
    14714232 bytes allocated, including 47145120 bytes reclaimed
303963552391
|#
