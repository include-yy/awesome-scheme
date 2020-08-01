(define mat (make-vector 15 #f))

(call-with-input-file "data.txt"
  (lambda (po)
    (let f ([i 0])
      (cond
       ((= i 15) #t)
       (else
	(vector-set! mat i (make-vector 15 0))
	(let g ([j 0])
	  (cond
	   ((= j 15) (f (+ i 1)))
	   (else
	    (let ([now (get-datum po)])
	      (vector-set! (vector-ref mat i) j now)
	      (g (+ j 1)))))))))))

(define-syntax mat-ref
  (syntax-rules ()
    [(_ m i j)
     (vector-ref (vector-ref m i) j)]))

(define-syntax mat-set!
  (syntax-rules ()
    [(_ m i j val)
     (vector-set! (vector-ref m i) j val)]))

(define make-nlist
  (lambda (n)
    (let f ([i 0])
      (cond
       ((= i n) '())
       (else
	(cons i (f (+ i 1))))))))

(define find-max-row-col-list
  (lambda (mat row-list col-list)
    (let f ([row row-list])
      (cond
       ((null? row) '())
       (else
	(let g ([ls col-list] [max-j 0] [max 0])
	  (cond
	   ((null? ls)
	    (cons (cons (car row) max-j) (f (cdr row))))
	   (else
	    (let ([now (mat-ref mat (car row) (car ls))])
	      (if (> now max)
		  (g (cdr ls) (car ls) now)
		  (g (cdr ls) max-j max)))))))))))

(define party
  (lambda (max-list)
    (let f ([single '()] [multi '()] [i 0])
      (cond
       ((= i 15) (values (reverse single) (reverse multi)))
       (else
	(let ([now-ls (filter (lambda (x) (= (cdr x) i)) max-list)])
	  (let ([now-ls-len (length now-ls)])
	    (cond
	     ((zero? now-ls-len) (f single multi (+ i 1)))
	     ((= now-ls-len 1) (f (append now-ls single) multi (+ i 1)))
	     (else
	      (f single (append now-ls multi) (+ i 1)))))))))))

(define inf (expt 2 32))

(define find-jmax
  (lambda (mat row col-list)
    (let f ([col col-list] [now (car col-list)])
      (cond
       ((null? col) (values now (mat-ref mat row now)))
       ((> (mat-ref mat row (car col)) (mat-ref mat row now))
	(f (cdr col) (car col)))
       (else
	(f (cdr col) now))))))

(define sub-least
  (lambda (mat col multi)
    (let f ([mult multi] [org-pair #f] [new-pair #f] [sub inf])
      (cond
       ((null? mult) (values new-pair org-pair))
       (else
	(let*-values ([(row) (car (car mult))]
		      [(j-max max-val) (find-jmax mat row col)])
	  (if (< (- (mat-ref mat row (cdr (car mult)))
		    (mat-ref mat row j-max)) sub)
	      (f (cdr mult) (car mult) (cons row j-max) (- (mat-ref mat row (cdr (car mult)))
							   (mat-ref mat row j-max)))
	      (f (cdr mult) org-pair new-pair sub))))))))
      
(define pro
  (lambda (mat row-ls col-ls total)
    (let f ([row row-ls] [col col-ls] [tot total])
      (cond
       ((= (length row) 1)
	(+ tot (mat-ref mat (car row) (car col))))
       (else
	(let ([divide-list (find-max-row-col-list mat row col)])
	  (let-values ([(single multi) (party divide-list)])
	    (cond
	     ((null? single)
	      (let* ([mult-col (map (lambda (x) (cdr x)) multi)]
		     [rest-col (remp (lambda (x) (member x mult-col)) col)])
		(let-values ([(new-pair org-pair) (sub-least mat rest-col multi)])
		  (f (remove (car new-pair) row)
		     (remove (cdr new-pair) col)
		     (+ tot (mat-ref mat (car new-pair) (cdr new-pair)))))))
	     (else
	      (let ([rem-row (map (lambda (x) (car x)) single)]
		    [rem-col (map (lambda (x) (cdr x)) single)])
		(f (remp (lambda (x) (member x rem-row)) row)
		   (remp (lambda (x) (member x rem-col)) col)
		   (fold-left (lambda (x y) (+ x (mat-ref mat (car y) (cdr y)))) tot single))))))))))))

;; the idea is wrong!
;;wrong answer: 13545
