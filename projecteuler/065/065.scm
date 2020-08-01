(define get-num
  (lambda (a)
    (let* ([stra (number->string a)]
	   [clsa (string->list stra)]
	   [lsa (map (lambda (x)
		       (- (char->integer x)
			  (char->integer #\0)))
		     clsa)])
      lsa)))
(define fact
  (lambda (n)
    (cond
     ((= n 0) 2)
     ((= n 1) 1)
     ((= n 2) 2)
     ((or (= (remainder n 3) 0)
	  (= (remainder n 3) 1))
      1)
     (else (* (/ (+ n 1) 3) 2)))))

(define neo
  (let f ([i 0])
    (cond
     ((= i 99) (fact 99))
     (else
      (+ (fact i) (/ (f (+ i 1))))))))

(apply + (get-num (numerator neo)))
