(define get-num
  (lambda (a)
    (let* ([stra (number->string a)]
	   [clsa (string->list stra)]
	   [lsa (map (lambda (x)
		       (- (char->integer x)
			  (char->integer #\0)))
		     clsa)])
      lsa)))

(define satis?
  (lambda (n)
    (not (zero? (remainder n 10)))))

(define list->digit
  (lambda (ls)
    (let f ([ls ls] [num 0])
      (cond
       ((null? ls) num)
       (else
	(f (cdr ls) (+ (* num 10) (car ls))))))))


(let pro ([i 1] [sum 0])
  (cond
   ((> i 100000000) sum)
   ((satis? i)
    (if (exists (lambda(x) (even? x))
		(get-num (+ i
			    (list->digit (reverse (get-num i))))))
	(pro (+ i 1) sum)
	(pro (+ i 1) (+ sum 1))))
   (else
    (pro (+ i 1) sum))))

#|
(time (let pro ...))
    26534 collections
    82.203125000s elapsed cpu time, including 0.343750000s collecting
    82.965982300s elapsed real time, including 0.494497800s collecting
    111752695624 bytes allocated, including 111750759848 bytes reclaimed
608720
|#
