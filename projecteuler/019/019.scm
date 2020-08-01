(define ping
  (vector 31 28 31 30 31 30 31 31 30 31 30 31))
(define run
  (vector 31 29 31 30 31 30 31 31 30 31 30 31))

(define arr-ref
  (lambda (arr n)
    (vector-ref arr (- n 1))))

(define run?
  (lambda (n)
    (or
     (and (= (remainder n 4) 0)
	  (not (= (remainder n 100) 0)))
     (= (remainder n 400) 0))))

(let loop ([year 1901] [sum 366] [accu 0])
  (cond
   ((> year 2000) accu)
   (else
    (let f ([month 1][days sum][acc 0])
      (let ([now-arr (if (run? year) run ping)])
	(cond
	 ((> month 12) (loop (+ year 1)
			     days
			     (+ accu acc)))
	 ((= (remainder days 7) 0)
	  (f (+ month 1) (+ days (arr-ref now-arr month)) (+ acc 1)))
	 (else
	  (f (+ month 1) (+ days (arr-ref now-arr month)) acc))))))))
