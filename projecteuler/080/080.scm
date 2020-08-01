(define get-num
  (lambda (a)
    (let* ([stra (number->string a)]
	   [clsa (string->list stra)]
	   [lsa (map (lambda (x)
		       (- (char->integer x)
			  (char->integer #\0)))
		     clsa)])
      lsa)))

(define list->int
  (lambda (ls)
    (let f ([ls ls] [sum 0])
      (cond
       ((null? ls) sum)
       (else
	(f (cdr ls) (+ (* sum 10) (car ls))))))))

(define ration?
  (lambda (n)
    (or (= n 1) (= n 4) (= n 9) (= n 16) (= n 25)
	(= n 36) (= n 49) (= n 64) (= n 81) (= n 100))))


(let pro ([i 1] [tot 0])
  (cond
   ((> i 100) tot)
   ((ration? i) (pro (+ i 1) tot))
   (else
    (let ([num (* i (expt 10 200))])
      (let-values ([(a b) (exact-integer-sqrt num)])
	(let f ([j 1] [lst (get-num a)] [sum 0])
	  (cond
	   ((> j 100) (pro (+ i 1) (+ tot sum)))
	   (else
	    (f (+ j 1) (cdr lst) (+ sum (car lst)))))))))))

#|
(time (let pro ...))
    no collections
    0.000000000s elapsed cpu time
    0.001339104s elapsed real time
    506408 bytes allocated
40886
|#
