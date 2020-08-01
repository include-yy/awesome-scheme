(define get-num
  (lambda (a)
    (let* ([stra (number->string a)]
	   [clsa (string->list stra)]
	   [lsa (map (lambda (x)
		       (- (char->integer x)
			  (char->integer #\0)))
		     clsa)])
      lsa)))

(define five-pow
  (lambda (n)
    (let ([ls (get-num n)])
      (fold-left (lambda(one n) (+ (expt n 5) one)) 0
		 ls))))
(define max
  (let ([nine (expt 9 5)])
    (let f ([i 1])
      (cond
       ((> (- (expt 10 i) 1) (* i nine)) (expt 10 i))
       (else
	(f (+ i 1)))))))

(let pro ([i 2][sum 0])
  (cond
   ((= i max) sum)
   ((= i (five-pow i))
    (pro (+ i 1) (+ sum i)))
   (else
    (pro (+ i 1) sum))))
