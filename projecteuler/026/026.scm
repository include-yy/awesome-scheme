(define cycles
  (lambda (n)
    (let loop ([i 0] [rest 1])
      (cond
       ((< i n) (loop (+ i 1) (remainder (* rest 10) n)))
       (else
	(let ([r0 rest])
	  (let f ([rest (remainder (* rest 10) n)] [sum 1])
	    (cond
	     ((= rest 0) 0)
	     ((= rest r0) sum)
	     (else
	      (f (remainder (* rest 10) n) (+ sum 1)))))))))))

(let pro ([i 2] [max (cons 0 0)])
  (cond
   ((> i 1000) max)
   (else
    (let ([now (cycles i)])
      (if (> now (cdr max))
	  (pro (+ i 1) (cons i now))
	  (pro (+ i 1) max))))))

#|
(time (let pro ...))
    no collections
    0.000000000s elapsed cpu time
    0.007624368s elapsed real time
    512 bytes allocated
(983 . 982)
|#
