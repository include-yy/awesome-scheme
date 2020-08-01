;;not 4 or 5
;;0123456789's length is 10, so the smallest's length maybe 8

;;0 1 2 3 6 7 8 9
(define get-num
  (lambda (a)
    (let* ([stra (number->string a)]
	   [clsa (string->list stra)]
	   [lsa (map (lambda (x)
		       (- (char->integer x)
			  (char->integer #\0)))
		     clsa)])
      lsa)))

(define vec-sec
  (call-with-input-file "p079_keylog.txt"
    (lambda (po)
      (let ([vec (make-vector 50 0)])
	(let f ([now (get-datum po)] [count 0])
	  (cond
	   ((eof-object? now) vec)
	   (else
	    (vector-set! vec count now)
	    (f (get-datum po) (+ count 1)))))))))

(define inorder?
  (lambda (ls num-ls)
    (cond
     ((null? ls) #t)
     (else
      (let ([now (member (car ls) num-ls)])
	(if now
	    (inorder? (cdr ls) now)
	    #f))))))

(define sec-lst
  (vector-map (lambda (x) (get-num x)) vec-sec))

(let pro ([i 10236789])
  (cond
   ((> i 99999999) #f)
   (else
    (let ([now (get-num i)])
      (let f ([j 0])
	(cond
	 ((= j 50) i)
	 ((inorder? (vector-ref sec-lst j) now)
	  (f (+ j 1)))
	 (else (pro (+ i 1)))))))))

#|
(time (let pro ...))
    8720 collections
    23.578125000s elapsed cpu time, including 0.093750000s collecting
    23.637719976s elapsed real time, including 0.137080116s collecting
    36752029040 bytes allocated, including 36750552232 bytes reclaimed
73162890
|#
