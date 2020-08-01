(define sum-of-divisors
  (lambda (n)
    (let f ([i 1] [sum 0])
      (cond
       ((= i n) sum)
       ((zero? (remainder n i))
	(f (+ i 1) (+ sum i)))
       (else
	(f (+ i 1) sum))))))

(let por ([i 2] [sum 0])
  (cond
   ((> i 10000) sum)
   (else
    (let ([now (sum-of-divisors i)])
      (if (and (= i (sum-of-divisors now))
	       (not (= i now)))
	  (por (+ i 1) (+ sum i))
	  (por (+ i 1) sum))))))
#|
(time (let por ...))
    no collections
    0.453125000s elapsed cpu time
    0.460377940s elapsed real time
    0 bytes allocated
31626
|#

