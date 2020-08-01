(define get-num-rev
  (lambda (a)
    (let f ([a a])
      (cond
       ((zero? a)'())
       (else
	(cons (remainder a 10)
	      (f (/(- a(remainder a 10))10))))))))

(define get-num
  (lambda (a)
    (let* ([stra (number->string a)]
	   [clsa (string->list stra)]
	   [lsa (map (lambda (x)
		       (- (char->integer x)
			  (char->integer #\0)))
		     clsa)])
      lsa)))

(define rev-num
  (lambda (n)
    (let f ([lst (get-num-rev n)] [sum 0])
      (cond
       ((null? lst) sum)
       (else
	(f (cdr lst) (+ (* sum 10) (car lst))))))))

(define palind?
  (lambda (n)
    (equal? (get-num n)
	    (get-num-rev n))))

(define Lych?
  (lambda (n)
    (let f ([now (+ n (rev-num n))] [iter 1])
      (cond
       ((>= iter 50) #f)
       ((palind? now) #t)
       (else
	(f (+ now (rev-num now)) (+ iter 1)))))))

(let pro ([i 1] [sum 0])
  (cond
   ((> i 10000) (- 10000 sum))
   ((Lych? i) (pro (+ i 1) (+ sum 1)))
   (else
    (pro (+ i 1) sum))))

#|
(time (let pro ...))
    10 collections
    0.125000000s elapsed cpu time, including 0.000000000s collecting
    0.122000544s elapsed real time, including 0.000309024s collecting
    41658896 bytes allocated, including 42107840 bytes reclaimed
249
|#
