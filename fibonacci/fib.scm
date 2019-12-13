(define fib-rec
  (lambda (n)
    (cond
     ((= n 1) 1)
     ((= n 2) 1)
     (else
      (+ (fib (- n 1)) (fib (- n 2)))))))

(define fib-ite
  (lambda (n)
    (let f ([a 1] [b 1] [n n])
      (cond
       ((= n 1) a)
       ((= n 2) b)
       (else
	(f b (+ a b) (- n 1)))))))

(define fib-mem
  (lambda (n)
    (let ([fib-vec (make-vector n 0)])
      (letrec ((A (lambda (n)
		    (cond
		     ((= n 0) 0)
		     ((= n 1) 1)
		     ((= n 2) 1)
		     (else
		      (+ (force (vector-ref fib-vec (- n 1)))
			 (force (vector-ref fib-vec (- n 2)))))))))
	(let f ([i 0])
	  (cond
	   ((= i n))
	   (else
	    (vector-set! fib-vec i (delay (A i)))
	    (f (+ i 1)))))
	(A n)))))

(define fib-cps
  (lambda (n k)
    (cond
     ((= n 1) (k 1))
     ((= n 2) (k 1))
     (else
      (fib-cps (- n 1)
	       (lambda (k2)
		 (fib-cps (- n 2) (lambda (k3) (k (+ k2 k3))))))))))

(define fib-cps2
  (lambda (n k)
    (cond
     ((= n 1) (k 1 0))
     ((= n 2) (k 1 0))
     (else
      (fib-cps2 (- n 1)
		(lambda (k2 k3)
		  (fib2 (- n 2) (lambda (x y)(k (+ k2 k3) (+ x y))))))))))

(define fib-stk
  (lambda (n)
    (cond
     ((= n 0) 0)
     ((or (= n 1) (= n 2)) 1)
     (else
      (let ([stack (make-vector n 0)])
	(vector-set! stack 0 2)
	(let pro ([ne n] [top 0] [count 0])
	  (cond
	   ((and (= top 0) (= (vector-ref stack 0) 0)) count)
	   ((or (= ne 1) (= ne 2))
	    (vector-set! stack top 0)
	    (cond
	     ((= (vector-ref stack (- top 1)) 1)
	      (pro (+ ne 1) (- top 1) (+ count 1)))
	     ((= (vector-ref stack (- top 1)) 0)
	      (pro (+ ne 2) (- top 1) (+ count 1)))))
	   ((= (vector-ref stack top) 2)
	    (vector-set! stack (+ top 1) 2)
	    (vector-set! stack top 1)
	    (pro (- ne 1) (+ top 1) count))
	   ((= (vector-ref stack top) 1)
	    (vector-set! stack (+ top 1) 2)
	    (vector-set! stack top 0)
	    (pro (- ne 2) (+ top 1) count))
	   ((= (vector-ref stack top) 0)
	    (cond
	     ((= (vector-ref stack (- top 1)) 1)
	      (pro (+ ne 1) (- top 1) count))
	     ((= (vector-ref stack (- top 1)) 0)
	      (pro (+ ne 2) (- top 1) count)))))))))))
