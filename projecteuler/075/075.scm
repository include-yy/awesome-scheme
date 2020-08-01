(define L 1500000)

(define max
  (let f ([i 1])
    (cond
     ((> (+ (* 2 i i) (* 2 i)) L) (- i 1))
     (else
      (f (+ i 1))))))

(define fin
  (let pro1 ([m 1] [lst '()])
    (cond
     ((> m max) lst)
     (else
      (let ([n (+ m 1)])
	(if (> (* 2 n (+ m n)) L) lst
	    (let f ([n n] [ls lst])
	      (let ([sum (* 2 n (+ m n))])
		(cond
		 ((> sum L)
		  (pro1 (+ m 1) ls))
		 ((= (gcd m n) 1)
		  (f (+ n 2) (cons sum ls)))
	       (else
		(f (+ n 2) ls)))))))))))

(define temp-vec (make-vector (+ L 1) 0))

(let f ([ls fin])
  (cond
   ((null? ls))
   (else
    (let ([now (car ls)])
      (vector-set! temp-vec now (+ (vector-ref temp-vec now) 1))
      (f (cdr ls))))))

(define new-ls
  (let f ([i 1][ls '()])
    (cond
     ((= i L) (reverse ls))
     ((zero? (vector-ref temp-vec i))
      (f (+ i 1) ls))
     (else
      (f (+ i 1)
	 (cons (cons i (vector-ref temp-vec i)) ls))))))

(define vec-now (make-vector (+ L 1) 0))

(let pro ([ls new-ls])
  (cond
   ((null? ls))
   (else
    (let* ([now (car ls)]
	   [orig (car now)]
	   [rest (cdr now)])
      (let f ([i orig])
	(cond
	 ((> i L) (pro (cdr ls)))
	 (else
	  (vector-set! vec-now i (+ (vector-ref vec-now i) rest))
	  (f (+ i orig)))))))))

(let f ([i 1] [count 0])
  (cond
   ((> i L) count)
   ((= 1 (vector-ref vec-now i))
    (f (+ i 1) (+ count 1)))
   (else (f (+ i 1) count))))

#|
(time (let pro ...))
    1 collection
    0.015625000s elapsed cpu time, including 0.000000000s collecting
    0.021177468s elapsed real time, including 0.004934172s collecting
    352 bytes allocated, including 789560 bytes reclaimed
161667
|#
