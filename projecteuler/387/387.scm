(define get-num
  (lambda (a)
    (reverse
     (let f ([a a])
       (cond
        ((zero? a)'())
	(else
	 (cons (remainder a 10)
	       (f (/(- a(remainder a 10))10)))))))))
;;0 ~ 9
(define ass-list '((1 1) (2 2) (3 3) (4 4) (5 5) (6 6) (7 7) (8 8) (9 9)))

(define upclass
  (lambda (alist)
    (cond
     ((null? alist) '())
     (else
      (let f ([i 0] [lst (car alist)])
	(cond
	 ((= i 10) (upclass (cdr alist)))
	 (else
	  (let ([upnum (+ (* 10 (car lst)) i)]
		[divider (+ (cadr lst) i)])
	    (if (= 0 (remainder upnum divider))
		(cons (list upnum divider)
		      (f (+ i 1) lst))
		(f (+ i 1) lst))))))))))
(define find-under
  (lambda (n ls)
    (cond
     ((>= (caar ls) n) '())
     (else
      (let ([now (upclass ls)])
	(append ls (find-under n now)))))))

(define prime?
  (lambda (n)
    (cond
     ((= n 1) #f)
     ((= n 2) #t)
     ((= 0 (remainder n 2)) #f)
     (else
      (let f ([i 3])
	(cond
	 ((> (* i i) n) #t)
	 ((= 0 (remainder n i)) #f)
	 (else
	  (f (+ i 2)))))))))
     
(define new (find-under (expt 10 13) ass-list))
(define new-list (filter (lambda (x) (prime? (/ (car x) (cadr x)))) new))

(define tail '(1 3 7 9))
(define add-prime
  (lambda (alist sum)
    (cond
     ((null? alist) sum)
     (else
      (let f ([i (caar alist)] [tal tail]
	      [nowsum 0])
	(cond
	 ((null? tal) (add-prime (cdr alist) (+ sum nowsum)))
	 (else
	  (let ([maybe (+ (* i 10) (car tal))])
	    (cond
	     ((prime? maybe)
	      (f i (cdr tal) (+ nowsum maybe)))
	     (else
	      (f i (cdr tal) nowsum)))))))))))

(define add-prime-list
  (lambda (alist lst)
    (cond
     ((null? alist) (reverse lst))
     (else
      (let f ([i (caar alist)] [tal tail]
	      [nowsum '()])
	(cond
	 ((null? tal) (add-prime-list (cdr alist) (append nowsum lst)))
	 (else
	  (let ([maybe (+ (* i 10) (car tal))])
	    (cond
	     ((prime? maybe)
	      (f i (cdr tal) (cons maybe nowsum)))
	     (else
	      (f i (cdr tal) nowsum)))))))))))

(add-prime new-list 0)
#|
(time (add-prime new-list ...))
    237 collections
    4.859375000s elapsed cpu time, including 0.000000000s collecting
    4.877848300s elapsed real time, including 0.007244900s collecting
    999808720 bytes allocated, including 1000071176 bytes reclaimed
696067597313468
|#
