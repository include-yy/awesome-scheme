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

(define place-ls
  (let f ([i 0])
    (cond
     ((= i 5) '())
     (else
      (cons i (f (+ i 1)))))))

(let pro ([row 0] [ls place-ls] [accu 0])
  (cond
   ((null? ls) 0)
   (else
    (let f ([now-ls ls] [now-max 0])
      (cond
       ((null? now-ls) now-max)
       (else
	(let* ([now-col (car now-ls)]
	       [curr (mat-ref mat row (car now-ls))]
	       [next (pro (+ row 1) (remove now-col ls) (+ accu curr))])
	  (if (> (+ curr next) now-max)
	      (f (cdr now-ls) (+ curr next))
	      (f (cdr now-ls) now-max)))))))))

;; too slow to slove this problem
