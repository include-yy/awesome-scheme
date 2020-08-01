(define get-num
  (lambda (a)
    (reverse
     (let f ([a a])
       (cond
        ((zero? a)'())
	(else
	 (cons (remainder a 10)
	       (f (/(- a(remainder a 10))10)))))))))
(define palind?
  (lambda (num)
    (let ([num-ls (get-num num)])
      (equal? (reverse num-ls) num-ls))))

(let process ([i 100] [biggest 0])
  (cond
   ((> i 999) biggest)
   (else
    (let f ([j 100] [big 0])
      (cond
       ((> j 999)
	(if (> big biggest)
	    (process (+ i 1) big)
	    (process (+ i 1) biggest)))
       (else
	(let ((now (* i j)))
	  (cond
	   ((palind? now)
	    (if (> now big)
		(f (+ j 1) now)
		(f (+ j 1) big)))
	   (else
	    (f (+ j 1) big))))))))))
#|
(time (let process ...))
    27 collections
    0.218750000s elapsed cpu time, including 0.000000000s collecting
    0.222798868s elapsed real time, including 0.000627372s collecting
    113261800 bytes allocated, including 113700288 bytes reclaimed
906609
|#
