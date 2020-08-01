(define get-num
  (lambda (a)
    (let* ([stra (number->string a)]
	   [clsa (string->list stra)]
	   [lsa (map (lambda (x)
		       (- (char->integer x)
			  (char->integer #\0)))
		     clsa)])
      lsa)))

(define n!
  (lambda (n)
    (cond
     ((= n 0) 1)
     ((= n 1) 1)
     (else
      (* n (n! (- n 1)))))))

(define chains
  (lambda (n)
    (let f ([n n] [ls (list n)])
      (let* ([new-n-ls (get-num n)]
	     [new-n (fold-left (lambda (x y) (+ x (n! y))) 0 new-n-ls)])
	(cond
	 ((member new-n ls) (length ls))
	 (else
	  (f new-n (cons new-n ls))))))))

(define one-million 1000000)

(let pro ([i 1] [sum 0])
  (cond
   ((= i one-million) sum)
   ((= (chains i) 60)
    (pro (+ i 1) (+ sum 1)))
   (else
    (pro (+ i 1) sum))))

#|
(time (let pro ...))
    3983 collections
    14.406250000s elapsed cpu time, including 0.062500000s collecting
    14.421956288s elapsed real time, including 0.098935188s collecting
    16766404128 bytes allocated, including 16765711536 bytes reclaimed
402
|#
