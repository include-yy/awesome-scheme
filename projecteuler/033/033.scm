(define cancel
  (let f ([i 10] [ls '()])
    (cond
     ((= i 100) ls)
     ((zero? (remainder i 10))
      (f (+ i 1) ls))
     (else
      (let g ([j 10] [ls ls])
	(cond
	 ((= j 100) (f (+ i 1) ls))
	 ((zero? (remainder j 10))
	  (g (+ j 1) ls))
	 ((>= i j)
	  (g (+ j 1) ls))
	 (else
	  (g (+ j 1) (cons (cons i j) ls)))))))))

(define get-num
  (lambda (a)
    (let* ([stra (number->string a)]
	   [clsa (string->list stra)]
	   [lsa (map (lambda (x)
		       (- (char->integer x)
			  (char->integer #\0)))
		     clsa)])
      lsa)))

(define dealfun
  (lambda (par)
    (let* ([fst (car par)]
	   [scd (cdr par)]
	   [fls (get-num fst)]
	   [sls (get-num scd)])
      (let ([common (cond ((or (member (car fls) sls) (member (cadr fls) sls))
			   (if (member (car fls) sls) (car fls) (cadr fls)))
			  (else #f))])
	(if (eq? common #f)
	    #f
	    (let ([nown (cond ((= (car fls) (cadr fls)) (car fls))
			       (else (car (remove common fls))))]
		  [deon (cond ((= (car sls) (cadr sls)) (car sls))
			      (else (car (remove common sls))))])
	      (if (= (/ nown deon) (/ fst scd)) #t #f)))))))

(apply * 
(let pro ([ls cancel])
  (cond
   ((null? ls) '())
   ((dealfun (car ls))
    (cons (/ (caar ls) (cdar ls)) (pro (cdr ls))))
   (else
    (pro (cdr ls)))))
)
;;1/100 => 100
