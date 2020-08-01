(define get-num
  (lambda (a)
    (let* ([stra (number->string a)]
	   [clsa (string->list stra)]
	   [lsa (map (lambda (x)
		       (- (char->integer x)
			  (char->integer #\0)))
		     clsa)])
      lsa)))

(let pro ([i 1] [max 1])
  (cond
   ((> i 100) max)
   (else
    (let f ([j 1] [curr max])
      (cond
       ((> j 100) (pro (+ i 1) curr))
       (else
	(let* ([now (expt i j)]
	       [lst (get-num now)]
	       [num (apply + lst)])
	  (if (> num curr)
	      (f (+ j 1) num)
	      (f (+ j 1) curr)))))))))

#|
(time (let pro ...))
    10 collections
    0.046875000s elapsed cpu time, including 0.000000000s collecting
    0.042105852s elapsed real time, including 0.000227328s collecting
    42740504 bytes allocated, including 42106616 bytes reclaimed
972
|#
