(define get-num
  (lambda (a)
    (let* ([stra (number->string a)]
	   [clsa (string->list stra)]
	   [lsa (map (lambda (x)
		       (- (char->integer x)
			  (char->integer #\0)))
		     clsa)])
      lsa)))

(define get-num-bu
  (lambda (a)
    (let ([ls (get-num a)])
      (cond
       ((= (length ls) 1)
	(cons 0 (cons 0 ls)))
       ((= (length ls) 2)
	(cons 0 ls))
       (else
	ls)))))

(define unique?
  (lambda(ls)
    (cond
     ((null? ls) #t)
     ((null? (cdr ls)) #t)
     (else
      (let ([fst (car ls)])
	(if (member fst (cdr ls))
	    #f
	    (unique? (cdr ls))))))))

(define remove-list
  (lambda (ls1 lso)
    (cond
     ((null? ls1) lso)
     (else
      (remove-list (cdr ls1)
		   (remove (car ls1) lso))))))

(define init-list
  (let f ([i 0])
    (cond
     ((= i 1000) '())
     ((unique? (get-num-bu i))
      (if (> i 100)
	  (cons (list i (remainder i 100)
		      (remove-list (get-num-bu i) '(0 1 2 3 4 5 6 7 8 9)))
		(f (+ i 1)))
	  (cons (list i (remainder i 100)
		      (remove-list (get-num-bu i) '(0 1 2 3 4 5 6 7 8 9)))
		(f (+ i 1)))))
     (else
      (f (+ i 1))))))

(define prime-vec (vector 2 3 5 7 11 13 17))

(define deal-lst
  (lambda (lst i)
    (let ([fst (car lst)] [num-lst (caddr lst)])
      (let f ([sec (cadr lst)] [now-num-lst num-lst])
	(cond
	 ((null? now-num-lst) '())
	 (else
	  (let ([third (car now-num-lst)])
	    (if (zero? (remainder (+ (* sec 10) third) (vector-ref prime-vec i)))
		(cons (list (+ (* fst 10) third)
			    (remainder (+ (* fst 10) third) 100)
			    (remove third num-lst))
		      (f sec (cdr now-num-lst)))
		(f sec (cdr now-num-lst))))))))))

(fold-left (lambda(x y) (+ x (car y))) 0 
	   (let pro ([lst init-list] [i 0])
	     (cond
	      ((= i 7) lst)
	      (else
	       (let f ([ls lst] [sum-lst '()])
		 (cond
		  ((null? ls) (pro sum-lst (+ i 1)))
		  (else
		   (f (cdr ls) (append sum-lst (deal-lst (car ls) i))))))))))
#|
(time (fold-left (lambda (...) ...) ...))
    58 collections
    0.156250000s elapsed cpu time, including 0.000000000s collecting
    0.162405988s elapsed real time, including 0.005488284s collecting
    246010888 bytes allocated, including 244657688 bytes reclaimed
16695334890
|#
