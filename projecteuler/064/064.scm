(define exact-sqrt
  (lambda (n)
    (cond
     ((not (integer? n))
      (assertion-violation 'exact-sqrt "not an integer" n))
     ((negative? n)
      (assertion-violation 'exact-sqrt "not a non-negative integer" n))
     ((= n 0) 0)
     (else
      (let* ([num-of-digits (string-length (number->string n))]
	     [digits-required (quotient (+ num-of-digits 1) 2)]
	     [base-case (if (zero? (remainder num-of-digits 2)) 3 1)])
	(let f ([i (* base-case (expt 10 (- digits-required 1)))])
	  (let ([now (* i i)]
		[next (* (+ i 1) (+ i 1))])
	    (cond
	     ((= now n) i)
	     ((and (< now n) (> next n)) i)
	     (else (f (+ i 1)))))))))))

(define sqrt-con-frac
  (lambda (n)
    (cond
     ((not (integer? n))
      (assertion-violation 'sqrt-con-frac "not an integer" n))
     ((negative? n)
      (assertion-violation 'sqrt-con-frac "not a non-negative integer" n))
     (else
      (let ([fst (exact-sqrt n)])
	(letrec
	    ((A (lambda (numer denomi)
		  (let* ([new-numer (- n (* denomi denomi))]
			 [curr-numer (/ new-numer numer)]
			 [up (+ denomi fst)]
			 [beside (quotient up curr-numer)]
			 [remain (- fst (remainder up curr-numer))])
		    (if (= (* 2 fst) beside)
			(list beside)
			(cons beside (A curr-numer remain)))))))
	  (cond
	   ((= (* fst fst) n) (list fst))
	   (else
	    (cons fst (A 1 fst))))))))))

(define con-frac
  (lambda (conls)
    (cond
     ((not (list? conls))
      (assertion-violation 'con-frac "not a list" conls))
     ((null? conls)
      (assertion-violation 'con-frac "null list" conls))
     (else
      (let f ([ls (reverse conls)] [sum 0])
	(cond
	 ((null? (cdr ls)) (+ (car ls) sum))
	 (else (f (cdr ls) (/ (+ sum (car ls)))))))))))

(define squ-list
  (let f ([i 1])
    (cond
     ((> (* i i) 10000) '())
     (else
      (cons (* i i) (f (+ i 1)))))))

(let pro ([i 1] [count 0])
  (cond
   ((> i 10000) count)
   ((member i squ-list) (pro (+ i 1) count))
   ((zero? (remainder (length (sqrt-con-frac i)) 2))
    (pro (+ i 1) (+ count 1)))
   (else
    (pro (+ i 1) count))))

#|
(time (let pro ...))
    2 collections
    0.015625000s elapsed cpu time, including 0.000000000s collecting
    0.019620804s elapsed real time, including 0.000107892s collecting
    6920992 bytes allocated, including 8464880 bytes reclaimed
1322
|#
