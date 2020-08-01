(define C
  (lambda (start)
    (let f ([n start] [count 1])
      (cond
       ((= n 1) count)
       (else
	(cond
	 ((= (remainder n 2) 0)
	  (f (/ n 2) (+ 1 count)))
	 ((= (remainder n 2) 1)
	  (f (+ (* 3 n) 1) (+ 1 count)))))))))

(let pro ([i 1] [max (cons 1 0)])
  (cond
   ((> i 1000000) max)
   (else
    (let ([now (C i)])
      (if (> now (cdr max))
	  (pro (+ i 1) (cons i now))
	  (pro (+ i 1) max))))))

#|
(time (let pro ...))
    no collections
    3.484375000s elapsed cpu time
    3.474162364s elapsed real time
    219960 bytes allocated
(837799 . 525)
|#
