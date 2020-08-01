(define fac
  (lambda (n)
    (cond
     ((< n 0) #f)
     ((= n 0) 1)
     ((= n 1) 1)
     (else
      (* n (fac (- n 1)))))))

(define weishu
  (let f ([i 1])
    (cond
     ((>= (- (expt 10 i) 1) (* i (fac 9))) i)
     (else
      (f (+ i 1))))))

(define get-num
  (lambda (a)
    (let* ([stra (number->string a)]
	   [clsa (string->list stra)]
	   [lsa (map (lambda (x)
		       (- (char->integer x)
			  (char->integer #\0)))
		     clsa)])
      lsa)))

(define fac!
  (lambda (ls)
    (fold-left (lambda (x y) (+ x (fac y))) 0
	       ls)))

(define max (* weishu (fac 9)))

(let pro ([i 3] [ls '()])
  (cond
   ((> i max) ls)
   ((= i (fac! (get-num i)))
    (pro (+ i 1) (cons i ls)))
   (else
    (pro (+ i 1) ls))))

#|
(time (let pro ...))
    339 collections
    1.484375000s elapsed cpu time, including 0.000000000s collecting
    1.484098972s elapsed real time, including 0.007943160s collecting
    1425154456 bytes allocated, including 1427151872 bytes reclaimed
(40585 145)
|#
