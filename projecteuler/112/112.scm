(define bouncy?
  (lambda (num)
    (if (<= num 100) #t
	(let* ([str-num (number->string num)]
	       [bound (string-length str-num)])
	  (let f ([i 0])
	    (cond
	     ((= i (- bound 1)) #t)
	     ((char=? (string-ref str-num i)
		      (string-ref str-num (+ i 1)))
	      (f (+ i 1)))
	     (else
	      (let ([judge (if (char<? (string-ref str-num i)
				  (string-ref str-num (+ i 1)))
			       char<=? char>=?)])
		(let g ([j (+ i 1)])
		  (cond
		   ((= j (- bound 1)) #t)
		   ((judge (string-ref str-num j)
			   (string-ref str-num (+ j 1)))
		    (g (+ j 1)))
		   (else #f)))))))))))

(let pro ([i 1] [sum 0])
  (cond
   ((bouncy? i)
    (if (= 99/100 (/ sum i)) i
	(pro (+ i 1) sum)))
   (else
    (if (= 99/100 (/ (+ sum 1) i)) i
	(pro (+ i 1) (+ sum 1))))))

#|
(time (let pro ...))
    178 collections
    0.687500000s elapsed cpu time, including 0.000000000s collecting
    0.688882600s elapsed real time, including 0.002348200s collecting
    749083168 bytes allocated, including 749608648 bytes reclaimed
1587000
|#
