(define get-num
  (lambda (a)
    (reverse
     (let f ([a a])
       (cond
        ((zero? a)'())
	(else
	 (cons (remainder a 10)
	       (f (/(- a(remainder a 10))10)))))))))

(define factorial
  (lambda (n)
    (cond
     ((< n 0) #f)
     ((= n 0) 1)
     ((= n 1) 1)
     (else
      (* n (factorial (- n 1)))))))

(let* ([a (factorial 100)]
       [lsa (get-num a)])
  (apply + lsa))
