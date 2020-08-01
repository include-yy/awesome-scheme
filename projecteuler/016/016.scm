(define get-num
  (lambda (a)
    (reverse
     (let f ([a a])
       (cond
        ((zero? a)'())
	(else
	 (cons (remainder a 10)
	       (f (/(- a(remainder a 10))10)))))))))

(define a (expt 2 1000))

(let ([lsa (get-num a)])
  (apply + lsa))

