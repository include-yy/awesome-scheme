(define amount
  (lambda (a)
    (lambda (b)
      (/ (* a b (+ a 1) (+ b 1)) 4))))

(define two-mi 2000000)
(define half (exact (floor (sqrt two-mi))))
(define two-for
  (lambda (n #;a-or-b)
    (/ (* n (+ n 1)) 2)))

(define two-lst
  (let f ([i 1])
    (let ([now (two-for i)])
      (cond
       ((> now two-mi) '())
       (else
	(cons now (f (+ i 1))))))))

(define two-vec
  (list->vector two-lst))

(define diff
  (lambda (n)
    (abs (- n two-mi))))

(define new-ls
  (let f ([lis two-lst])
    (let ([now (car lis)])
      (cond
       ((> (* now now) two-mi) (cons (list now now (* now now)) '()))
       (else
	(let g ([ls lis])
	  (cond
	   ((null? ls) (f (cdr lis)))
	   ((> (* (car ls) now) two-mi)
	    (cons (list (car ls) now (* (car ls) now)) (f (cdr lis))))
	   (else
	    (cons (list (car ls) now (* (car ls) now)) (g (cdr ls)))))))))))

(car (sort (lambda (a b) (< (diff (caddr a)) (diff (caddr b)))) new-ls))
;;(3003 666 1999998)
