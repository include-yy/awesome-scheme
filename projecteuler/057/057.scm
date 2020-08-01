(define con-frac
  (lambda (conls)
    (cond
     ((not (list? conls))
      (assertion-violation 'con-frac "not a list" conls))
     ((null? conls)
      (assertion-violation 'con-frac "null list" conls))
     (else
      (let f ([ls conls] [sum 0])
	(cond
	 ((null? (cdr ls)) (+ (car ls) sum))
	 (else (f (cdr ls) (/ (+ sum (car ls)))))))))))

(let pro ([i 1] [ls (list 2 1)] [sum 0])
  (cond
   ((> i 1000) sum)
   (else
    (let* ([num (con-frac ls)]
	   [numer (numerator num)]
	   [deno (denominator num)])
      (if (> (string-length (number->string numer))
	     (string-length (number->string deno)))
	  (pro (+ i 1) (cons 2 ls) (+ sum 1))
	  (pro (+ i 1) (cons 2 ls) sum))))))

#|
(time (let pro ...))
    56 collections
    28.359375000s elapsed cpu time, including 0.000000000s collecting
    28.377119836s elapsed real time, including 0.001990008s collecting
    238660832 bytes allocated, including 238163992 bytes reclaimed
153
|#
