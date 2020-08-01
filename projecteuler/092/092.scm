(define get-num
  (lambda (a)
    (let* ([stra (number->string a)]
	   [clsa (string->list stra)]
	   [lsa (map (lambda (x)
		       (- (char->integer x)
			  (char->integer #\0)))
		     clsa)])
      lsa)))

(let pro ([i 1] [sum 0])
  (cond
   ((= i 10000000) sum)
   (else
    (let f ([j i])
      (let ([new (fold-left (lambda (n x) (+ n (* x x))) 0 (get-num j))])
	(cond
	 ((= new 1) (pro (+ i 1) sum))
	 ((= new 89) (pro (+ i 1) (+ sum 1)))
	 (else
	  (f new))))))))

#|
(time (let pro ...))
    8307 collections
    23.156250000s elapsed cpu time, including 0.171875000s collecting
    23.170219400s elapsed real time, including 0.146278800s collecting
    34993867768 bytes allocated, including 34993794752 bytes reclaimed
8581146
|#
