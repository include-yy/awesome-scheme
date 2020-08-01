(define get-num
  (lambda (a)
    (reverse
     (let f ([a a])
       (cond
        ((zero? a)'())
	(else
	 (cons (remainder a 10)
	       (f (/(- a(remainder a 10))10)))))))))

(define satis?
  (lambda (ls)
    (cond
     ((and (= 9 (length ls))
	   (equal? (sort < ls) '(1 2 3 4 5 6 7 8 9)))
      #t)
     (else #f))))

(define make-1to9
  (lambda (n)
    (let f ([i 1] [ls '()] [sum '()])
      (cond
       ((>= (length ls) 9)
	(values ls
		(string->number
		 (fold-left
		  (lambda (x y) (string-append x y))
		  "" (map number->string (reverse sum))))))
       (else
	(f (+ i 1)
	   (append (get-num (* i n)) ls)
	   (cons (* i n) sum)))))))

(let pro ([i 1] [max 0])
  (cond
   ((> i 9999) max)
   (else
    (let-values ([(nst val) (make-1to9 i)])
      (if (satis? nst)
	  (if (> val max)
	      (pro (+ i 1) val)
	      (pro (+ i 1) max))
	  (pro (+ i 1) max))))))

#|
(time (let pro ...))
    4 collections
    0.062500000s elapsed cpu time, including 0.000000000s collecting
    0.055651848s elapsed real time, including 0.000387168s collecting
    16992104 bytes allocated, including 16838072 bytes reclaimed
932718654
|#
