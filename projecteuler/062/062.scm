(define get-num
  (lambda (a)
    (let* ([stra (number->string a)]
	   [clsa (string->list stra)]
	   [lsa (map (lambda (x)
		       (- (char->integer x)
			  (char->integer #\0)))
		     clsa)])
      lsa)))

(define cube-list
  (let f ([i 4642])
    (cond
     ((> (expt i 3)  1000000000000) '());12
     (else
      (cons (expt i 3) (f (+ i 1)))))))

(let pro ([cur cube-list][max (cons 0 0)])
  (cond
   ((null? (cdr cur)) max)
   (else
    (let* ([str1 (number->string (car cur))]
	   [len (string-length str1)]
	   [las1 (sort char<? (string->list str1))])
      (let f ([ls cube-list] [now 0])
	(cond
	 ((null? ls)
	  (if (> now (cdr max))
	      (pro (cdr cur) (cons (car cur) now))
	      (pro (cdr cur) max)))
	 (else
	  (let ([nowstr (number->string (car ls))])
	    (if (= (string-length nowstr) len)
		(if (equal? las1 (sort char<? (string->list nowstr)))
		    (f (cdr ls) (+ now 1))
		    (f (cdr ls) now))
		(if (> (string-length nowstr) len)
		    (if (> now (cdr max))
			(pro (cdr cur) (cons (car cur) now))
			(pro (cdr cur) max))
		    (f (cdr ls) now)))))))))))

#|
(time (let pro ...))
    6124 collections
    26.265625000s elapsed cpu time, including 0.109375000s collecting
    26.372579492s elapsed real time, including 0.118396152s collecting
    25804662152 bytes allocated, including 25805436112 bytes reclaimed
(127035954683 . 5)
|#
