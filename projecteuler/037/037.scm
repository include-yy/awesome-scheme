(define range 10000000)
(define sievebound (quotient (- range 1) 2))
(define crosslimit (quotient (- (exact (floor (sqrt range))) 1) 2))

(define prime (make-bytevector sievebound 1))
(define prime-ref
  (lambda (pme i)
    (bytevector-u8-ref pme (- i 1))))
(define prime-set!
  (lambda (pme i arg)
    (bytevector-u8-set! pme (- i 1) arg)))
    
(let filt ([i 1])
  (cond
   ((> i sievebound))
   ((= (prime-ref prime i) 1)
    (let f ([j (* 2 i (+ i 1))])
      (cond
       ((> j sievebound) (filt (+ i 1)))
       (else
	(prime-set! prime j 0)
	(f (+ j (+ (* i 2) 1)))))))
   (else
    (filt (+ i 1)))))

(define prime-r?
  (lambda (n)
    (cond
     ((<= n 0) #f)
     ((= n 1) #f)
     ((= n 2) #t)
     ((= (remainder n 2) 0) #f)
     ((= (prime-ref prime (/ (- n 1) 2)) 1) #t)
     (else #f))))


(define r-t-l
  (lambda (n)
    (quotient n 10)))

(define l-t-r
  (lambda (n)
    (let ([stl (number->string n)])
      (string-set! stl 0 #\0)
      (string->number stl))))

(define base (list 2 3 5 7))

(define left-tr-lst
  (let f ([i 1] [ls base] [curr base])
    (cond
     ((>= i 7) ls)
     (else
      (let g ([lst curr] [cur '()])
	(cond
	 ((null? lst) (f (+ i 1)
			 (append cur ls)
			 cur))
	 (else
	  (let h ([j 1] [atom (car lst)] [get '()])
	    (let ([prim (+ (* j (expt 10 i)) atom)])
	      (cond
	       ((> j 9) (g (cdr lst) (append get cur)))
	       ((prime-r? prim) 
		(h (+ j 1) atom (cons prim get)))
	       (else
		(h (+ j 1) atom get))))))))))))

(define right-yes?
  (lambda (n)
    (cond
     ((= n 0) #t)
     ((prime-r? n)
      (right-yes? (r-t-l n)))
     (else #f))))
      
(- 
(apply + 
(let pro ([ls left-tr-lst] [res '()])
  (cond
   ((null? ls) res)
   ((right-yes? (car ls))
    (pro (cdr ls) (cons (car ls) res)))
   (else
    (pro (cdr ls) res))))
)
(apply + base))
;;748317
