(define table (make-vector 1001 0))

(let f ([n 1])
  (let ([m (+ n 1)])
    (cond
     ((> (* 2 (+ (* m m) (* m n))) 1000) #t)
     (else
      (let g ([me m])
	(cond
	 ((> (* 2 (+ (* me me) (* me n))) 1000) (f (+ n 1)))
	 ((even? n)
	  (if (= 1 (gcd n me))
	      (begin (vector-set!
		      table
		      (* 2 (+ (* me me) (* me n))) 1)
		     (g (+ me 2)))
	      (g (+ me 2))))
	 (else
	  (if (= 1 (gcd n me))
	      (begin (vector-set!
		      table
		      (* 2 (+ (* me me) (* me n))) 1)
		     (g (+ me 2)))
	      (g (+ me 2))))))))))
	 
				  

(define exlst
  (let f ([i 0] [ls '()])
    (cond
     ((> i 1000) (reverse ls))
     ((= (vector-ref table i) 1)
      (f (+ i 1) (cons i ls)))
     (else
      (f (+ i 1) ls)))))

(let pro ([i 12] [max (cons 12 1)])
  (cond
   ((> i 1000) max)
   (else
    (let ([now-max (fold-left (lambda (x y)
				(cond
				 ((zero? (remainder i y))
				  (+ x 1))
				 (else x))) 0 exlst)])
      (if (> now-max (cdr max))
	  (pro (+ i 1) (cons i now-max))
	  (pro (+ i 1) max))))))
;;(840 . 8) => 840
