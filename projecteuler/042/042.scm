(define tn
  (lambda (n)
    (* n (+ n 1) 1/2)))

(define 1to100
  (let f ([i 1])
    (cond
     ((> i 100) '())
     (else
      (cons (tn i) (f (+ i 1)))))))

(define str-to-digit
  (lambda (str)
    (let ([sls (string->list str)])
      (fold-left (lambda (x y) (+ x (- (char->integer y) (char->integer #\A) -1))) 0 sls))))

(call-with-input-file "p042_words.txt"
  (lambda (po)
    (let pro ([str (get-datum po)] [sum 0])
      (cond
       ((eof-object? str) sum)
       (else
	(get-char po)
	(if (member (str-to-digit str) 1to100)
	    (pro (get-datum po) (+ sum 1))
	    (pro (get-datum po) sum)))))))
;;162
