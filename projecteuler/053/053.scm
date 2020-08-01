(define n!
  (lambda (n)
    (if (= n 0)
	1
	(let f ([i 1] [pow 1])
	  (cond
	   ((= n i) (* pow n))
	   (else
	    (f (+ i 1) (* pow i))))))))

(define C
  (lambda (m n)
    (/ (n! m) (n! n) (n! (- m n)))))

(define one-million 1000000)

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

#|
(time (let pro ...))
    no collections
    0.000000000s elapsed cpu time
    0.003487176s elapsed real time
    1221296 bytes allocated
4075
|#
