(define get-num
  (lambda (a)
    (let* ([stra (number->string a)]
	   [clsa (string->list stra)]
	   [lsa (map (lambda (x)
		       (- (char->integer x)
			  (char->integer #\0)))
		     clsa)])
      lsa)))

(let pro ([i 1] [index 0] [bs 1] [ls '()])
  (cond
   ((> index 1000000) ls)
   (else
    (let* ([now-list (get-num i)]
	   [leng (length now-list)])
      (cond
       ((>= (+ index leng) bs)
	(pro (+ i 1)
	     (+ index leng)
	     (* bs 10)
	     (cons (list-ref now-list (- bs index 1))
		   ls)))
       (else
	(pro (+ i 1)
	     (+ index leng)
	     bs
	     ls)))))))

#|
(time (let pro ...))
    24 collections
    0.109375000s elapsed cpu time, including 0.000000000s collecting
    0.106527588s elapsed real time, including 0.000747252s collecting
    100452176 bytes allocated, including 101043904 bytes reclaimed
(1 2 7 3 5 1 1) => 210
|#
