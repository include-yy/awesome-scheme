(define fun-list (make-list 10 (lambda (x) #t)))

(begin
  (set-car! (list-tail fun-list 0)
	    (lambda (ls) (= (list-ref ls 0) 10)))
  (set-car! (list-tail fun-list 4)
	    (lambda (ls) (= (+ (list-ref ls 0)
			       (list-ref ls 1))
			    (+ (list-ref ls 3)
			       (list-ref ls 4)))))
  (set-car! (list-tail fun-list 6)
	    (lambda (ls) (= (+ (list-ref ls 2)
			       (list-ref ls 3))
			    (+ (list-ref ls 5)
			       (list-ref ls 6)))))
  (set-car! (list-tail fun-list 8)
	    (lambda (ls) (= (+ (list-ref ls 4)
			       (list-ref ls 5))
			    (+ (list-ref ls 7)
			       (list-ref ls 8)))))
  (set-car! (list-tail fun-list 9)
	    (lambda (ls) (= (+ (list-ref ls 6)
			       (list-ref ls 7))
			    (+ (list-ref ls 1)
			       (list-ref ls 9))))))

(define-syntax pop
  (syntax-rules()
    [(_ ls)
     (let ([len (length ls)])
       (cond
	((null? ls) '())
	((= len 1) (let ([now (car ls)]) (set! ls '()) now))
	(else
	 (let* ([now-ls (list-tail ls (- len 1))]
		[prev-ls (list-tail ls (- len 2))]
		[compond (car now-ls)])
	   (set-cdr! prev-ls '())
	   compond))))]))

(define remove-list
  (lambda (org tar)
    (cond
     ((null? tar) org)
     (else
      (remove-list (remove (car tar) org) (cdr tar))))))

(define 1-to-10
  (let f ([i 1])
    (cond
     ((> i 10) '())
     (else
      (cons i (f (+ i 1)))))))

(define res
(let pro ([search (list '())]
	  [sols '()])
  (cond
   ((= (length search) 0) sols)
   (else
    (let* ([c (pop search)]
	   [left (remove-list 1-to-10 c)])
      (if (zero? (length left))
	  (pro search (cons c sols))
	  (let g ([ls left][sch search])
	    (cond
	     ((null? ls) (pro sch sols))
	     (((list-ref fun-list (length c))
	       (append c (list (car ls))))
	      (g (cdr ls) (append sch (list (append c (list (car ls)))))))
	     (else
	      (g (cdr ls) sch)))))))))
)

#|
(time (let pro ...))
    no collections
    0.015625000s elapsed cpu time
    0.003178596s elapsed real time
    1676584 bytes allocated
((10 1 3 6 5 7 2 8 4 9)
 (10 1 5 2 9 4 3 6 7 8)
 (10 3 1 9 4 8 2 7 5 6)
 (10 5 1 8 7 6 3 4 9 2))
|#

(map (lambda (ls)
       (let-syntax
	   ([yy (identifier-syntax list-ref)])
	 (list (yy ls 0) (yy ls 1) (yy ls 2)
	       (yy ls 3) (yy ls 2) (yy ls 4)
	       (yy ls 5) (yy ls 4) (yy ls 6)
	       (yy ls 7) (yy ls 6) (yy ls 8)
	       (yy ls 9) (yy ls 8) (yy ls 1)))) res)
#|
((10 1 3 6 3 5 7 5 2 8 2 4 9 4 1)
 (10 1 5 2 5 9 4 9 3 6 3 7 8 7 1)
 (10 3 1 9 1 4 8 4 2 7 2 5 6 5 3)
 (10 5 1 8 1 7 6 7 3 4 3 9 2 9 5))
|#
