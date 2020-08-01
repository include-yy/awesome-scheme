(define 1-to-9
  (let ([lst '(1 2 3 4 5 6 7 8 9)])
    (lambda (str)
      (let ([me (sort < (map (lambda (x)
			     (- (char->integer x)
				(char->integer #\0)))
			   (string->list str)))])
	(equal? lst me)))))

(time
 (let pro ([k 41][a 102334155][b 165580141])
   (let* ([now (number->string b)]
	  [len (string-length now)])
     (let ([lst (substring now (- len 9) len)])
       (if (1-to-9 lst)
	   (let ([fst (substring now 0 9)])
	     (if (1-to-9 fst) k
		 (pro (+ k 1) b (+ a b))))
	   (pro (+ k 1) b (+ a b))))))
)

;; very slow

