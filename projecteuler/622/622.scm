(define list-merge
  (lambda (ls1 ls2)
    (let f ([i 1] [l1 ls1] [l2 ls2])
      (cond
       ((and (null? l2) (null? l1)) '())
       ((odd? i)
	(cons (car l1) (f (+ i 1) (cdr l1) l2)))
       (else
	(cons (car l2) (f (+ i 1) l1 (cdr l2))))))))

(define list-part
  (lambda (ls)
    (let ([len (- (/ (length ls) 2) 1)])
      (let f ([i 0] [ls-now ls])
	(cond
	 ((= i len)
	  (let ([rest (cdr ls-now)])
	    (set-cdr! ls-now '())
	    (values ls rest)))
	 (else
	  (f (+ i 1) (cdr ls-now))))))))

(define make-mylist
  (lambda (n)
    (let f ([i 1])
      (cond
       ((> i n) '())
       (else
	(cons i (f (+ i 1))))))))

(define S
  (lambda (n)
    (let* ([ls (make-mylist n)]
	   [org-ls (list-copy ls)])
      (let f ([now-ls (call-with-values (lambda () (list-part ls)) (lambda (ls1 ls2) (list-merge ls1 ls2)))] [tot 1])
	(cond
	 ((equal? org-ls now-ls) tot)
	 (else
	  (f (call-with-values (lambda () (list-part now-ls)) (lambda (ls1 ls2) (list-merge ls1 ls2)))
	     (+ tot 1))))))))


(do ([i 2 (+ i 2)] [tot 0 (if (= (S i) 60) (+ tot i) tot)]) ((> i 10000) tot))

;;too too slow
