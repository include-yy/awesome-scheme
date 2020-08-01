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

(define find-max-and-its-index
  (lambda (vec dim)
    (cond
     ((zero? dim)
      (assertion-violation "div is zero" dim 'find-max-and-its-index))
     (else
      (let f ([i 0] [max-val (vector-ref vec 0)] [max-index 0])
	(cond
	 ((= i dim)
	  (values max-val max-index))
	 (else
	  (let ([now-val (vector-ref vec i)])
	    (if (> now-val max-val)
		(f (+ i 1) now-val i)
		(f (+ i 1) max-val max-index))))))))))

(define find-max&index
  (lambda (vec)
    (find-max-and-its-index vec (vector-length vec))))

(define find-max&index-ex
  (lambda (vec rest-col)
    (let ([len (vector-length vec)])
      (let ([fst-j (let f ([i 0]) (cond ((not (zero? (vector-ref rest-col i))) i) (else (f (+ i 1)))))])
	(let f ([i fst-j] [max-val (vector-ref vec fst-j)] [max-index fst-j])
	  (cond
	   ((= i len)
	    (values max-val max-index))
	   ((not (zero? (vector-ref rest-col i)))
	    (let ([now-val (vector-ref vec i)])
	      (if (>= now-val max-val)
		  (f (+ i 1) now-val i)
		  (f (+ i 1) max-val max-index))))
	   (else
	    (f (+ i 1) max-val max-index))))))))
	
(define mat-5
  (vector (vector   7  53 183 439 863)
	  (vector 497 383 563  79 973)
	  (vector 287  63 343 169 583)
	  (vector 627 343 773 959 943)
	  (vector 767 473 103 699 303)))

(define line-unique?
  (lambda (vec)
    (let ([vec-ls (vector->list vec)])
      (let f ([ls vec-ls])
	(cond
	 ((null? (cdr ls)) #t)
	 ((member (car ls) (cdr ls)) #f)
	 (else
	  (f (cdr ls))))))))
;; 2 and 6th line has repete element

(define vector-null?
  (lambda (vec)
    (let ([len (vector-length vec)])
      (let f ([i 0])
	(cond
	 ((= i len) #t)
	 ((not (zero? (vector-ref vec i))) #f)
	 (else (f (+ i 1))))))))

(define me-now
(let-syntax ([vr (syntax-rules ()
		   [(_ vec i)
		    (vector-ref vec i)])]
	     [vs! (syntax-rules ()
		    [(_ vec i new)
		     (vector-set! vec i new)])])
  (let pro ([col-vec (make-vector 15 1)]
	    [have-selected (make-vector 15 #f)]
	    [row 0])
    (cond
     ((vector-null? col-vec) have-selected)
     (else
      (let-values ([(max-j j) (find-max&index-ex (vector-ref mat row) col-vec)])
	(vector-set! have-selected row (cons j max-j))
	(vector-set! col-vec j 0)
	(pro col-vec have-selected (+ row 1)))))))
)

(define now-val
  (lambda (vec-now)
    (fold-left (lambda (x y) (+ x (cdr y))) 0 (vector->list vec-now))))

(define final
(let-syntax ([vr (syntax-rules ()
		   [(_ vec i)
		    (vector-ref vec i)])]
	     [vs! (syntax-rules ()
		    [(_ vec i new)
		     (vector-set! vec i new)])])
  (let pro ([col-vec (make-vector 15 1)]
	    [have-selected (make-vector 15 #f)]
	    [row 0])
    (cond
     ((vector-null? col-vec) (deal have-selected 14))
     (else
      (let-values ([(max-j j) (find-max&index-ex (vector-ref mat row) col-vec)])
	(vector-set! col-vec j 0)
	(vector-set! have-selected row (cons j max-j))
	(if (= row 0)
	    (pro col-vec have-selected (+ row 1))
	    (pro col-vec (deal have-selected row) (+ row 1))))))))
)

(define deal
  (lambda (hv-ls seleted)
    (let ([len (+ seleted 1)])
      (let f ([sel seleted] [iter 0])
	(let ([my-vec (make-vector len 0)])
	  (let g ([i 0])
	    (cond
	     ((= i len))
	     ((= i seleted)
	      (vector-set! my-vec i 0)
	      (g (+ i 1)))
	     (else
	      (vector-set! my-vec i (- (+ (mat-ref mat i (car (vector-ref hv-ls sel)))
					  (mat-ref mat sel (car (vector-ref hv-ls i))))
				       (+ (cdr (vector-ref hv-ls i))
					  (cdr (vector-ref hv-ls sel)))))
	      (g (+ i 1)))))
	  (let ([my-max (let g ([i 0] [max-i 0])
			  (cond
			   ((= i len) max-i)
			   ((= i (car (vector-ref hv-ls sel)))
			    (g (+ i 1) max-i))
			   ((> (vector-ref my-vec i) (vector-ref my-vec max-i))
			    (g (+ i 1) i))
			   (else
			    (g (+ i 1) max-i))))])
	    (cond
	     ((< (vector-ref my-vec my-max) 0)
	      hv-ls)
	     ((and (= (vector-ref my-vec my-max) 0)
		   (> iter 1000))
	      hv-ls)
	     (else
	      (let ([org-col (car (vector-ref hv-ls sel))]
		    [new-col (car (vector-ref hv-ls my-max))])
		(vector-set! hv-ls my-max (cons org-col (mat-ref mat my-max org-col)))
		(vector-set! hv-ls sel (cons new-col (mat-ref mat sel new-col)))
		(f my-max (+ iter 1)))))))))))


(let f ([fin final] [i 1] [iter 0])
  (cond
   ((= iter 100) fin)
   ((= i 14) (f fin 1 (+ iter 1)))
   (else
    (let ([now (deal fin i)])
      (cond
       ((equal? now fin)
	(f fin (+ i 1) iter))
       (else
	(f now 1)))))))

;;failed again :p
;; 13892
