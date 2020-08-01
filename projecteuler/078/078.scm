(define sum-of-divisors
  (lambda (n)
    (cond
     ((= n 1) 1)
     (else
      (let loop ([i 2] [n n] [sum 1])
	(cond
	 ((and (<= (* i i) n) (> n 1))
	  (cond
	   ((zero? (remainder n i))
	    (let f ([p (* i i)] [num (/ n i)])
	      (cond
	       ((zero? (remainder num i))
		(f (* p i) (/ num i)))
	       (else
		(if (= i 2)
		    (loop 3 num (* sum (- p 1)))
		    (loop (+ i 2) num (/ (* sum (- p 1)) (- i 1))))))))
	   (else
	    (if (= i 2)
		(loop 3 n sum)
		(loop (+ i 2) n sum)))))
	 (else
	  (if (= n 1)
	      sum
	      (* (+ n 1) sum)))))))))


(begin
(define limit 100000)
(define div-vec (make-vector (+ limit 1) 0))
(define p-vec (make-vector (+ limit 1) 0))
(vector-set! p-vec 0 1)
)


(begin
(let f ([i 1])
  (cond
   ((> i limit))
   (else
    (vector-set! div-vec i (sum-of-divisors i))
    (f (+ i 1)))))

(let f ([i 1])
  (cond
   ((> i limit))
   (else
    (let g ([k 0] [count 0])
      (cond
       ((= k i)
	(vector-set! p-vec i (/ count i))
	(f (+ i 1)))
       (else
	(g (+ k 1) (+ count 
		      (* (vector-ref div-vec (- i k))
			 (vector-ref p-vec k))))))))))
)
#|
(time (begin (let f ...) ...))
    202732 collections
    775.015625000s elapsed cpu time, including 5.890625000s collecting
    781.002578752s elapsed real time, including 6.207816752s collecting
    861899133592 bytes allocated, including 861889101488 bytes reclaimed
#t
|#

(let f ([i 1])
  (cond
   ((> i limit) #f)
   ((zero? (remainder (vector-ref p-vec i) 1000000)) i)
   (else
    (f (+ i 1)))))
;;55374
