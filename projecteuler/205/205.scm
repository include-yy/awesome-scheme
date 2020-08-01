(define n!
  (lambda (n)
    (cond
     ((< n 0) #f)
     ((= n 0) 1)
     ((= n 1) 1)
     (else
      (* n (n! (- n 1)))))))

(define get-n!
  (lambda (ls)
    (let f ([now (car ls)] [rst (cdr ls)] [count 1])
      (cond
       ((null? rst) (cons count '()))
       ((= now (car rst))
	(f now (cdr rst) (+ count 1)))
       (else
	(cons count (f (car rst) (cdr rst) 1)))))))


(define peter (make-vector 37 0))
(define colin (make-vector 37 0))

(define four-side (list 4 3 2 1))
(define six-side (list 6 5 4 3 2 1))

(define find-for
  (lambda (n side)
    (let f ([n n] [side side] [ls '()])
      (cond
       ((null? side)
	(if (not (zero? n)) '()  (list ls)))
       ((< n (car side))
	(f n (cdr side) ls))
;;       ((= n (car side)) (list (cons (car side) ls)))
       (else
	(append (f (- n (car side)) side (cons (car side) ls))
		(f n (cdr side) ls)))))))

(define get-for
  (lambda (n side len)
    (let f ([lst (find-for n side)])
      (cond
       ((null? lst) '())
       ((not (= (length (car lst)) len))
	(f (cdr lst)))
       (else
	(cons (get-n! (car lst)) (f (cdr lst))))))))

(define possb
  (lambda (n lss p)
    (let f ([ls lss] [accu 0])
      (cond
       ((null? ls) (* accu (expt p n)))
       (else
	(f (cdr ls) (+ accu (/ (n! n)
			       (fold-left (lambda (z x)
					    (* z (n! x)))
					  1 (car ls))))))))))

(let pro1 ([amount 9] [len 9] [side four-side])
  (cond
   ((> amount 36))
   (else
    (let ([now (get-for amount side len)])
      (vector-set! peter amount (possb len now 1/4))
      (pro1 (+ amount 1) len side)))))

(let pro2 ([amount 6] [len 6] [side six-side])
  (cond
   ((> amount 36))
   (else
    (let ([now (get-for amount side len)])
      (vector-set! colin amount (possb len now 1/6))
      (pro2 (+ amount 1) len side)))))

(let pro3 ([i 9] [ps 0.0])
  (cond
   ((> i 36) ps)
   (else
    (let ([now (vector-ref peter i)])
      (let f ([j 0] [acc-now 0.0])
	(cond
	 ((= i j) (pro3 (+ i 1) (+ ps (* now acc-now))))
	 (else
	  (f (+ j 1) (+ acc-now (vector-ref colin j))))))))))

#|
(time (begin (let pro1 ...) ...))
    1 collection
    0.000000000s elapsed cpu time, including 0.000000000s collecting
    0.005296700s elapsed real time, including 0.000139600s collecting
    4429184 bytes allocated, including 4132944 bytes reclaimed
0.5731440767829801 
=> 0.5731441
|#
