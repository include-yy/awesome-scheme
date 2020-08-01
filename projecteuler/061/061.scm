(begin
(define tri
  (lambda (n)
    (* n (+ n 1) 1/2)))
(define squ
  (lambda (n)
    (* n n)))
(define penta
  (lambda (n)
    (* n (- (* n 3) 1) 1/2)))
(define hex
  (lambda (n)
    (* n (- (* n 2) 1))))
(define hept
  (lambda (n)
    (* n (- (* n 5) 3) 1/2)))
(define oct
  (lambda (n)
    (* n (- (* n 3) 2))))
)

(define per-list
  (lambda (pro)
    (let f ([i 1])
      (let ([now (pro i)])
	(cond
	 ((> now 10000) '())
	 ((>= now 1000)
	  (let ([p (remainder now 100)])
	    (if (>= p 10)
		(cons now (f (+ i 1)))
		(f (+ i 1)))))
	 (else (f (+ i 1))))))))

(define per-pro
  (list tri squ penta hex hept oct))

(define all-list
  (let f ([ls per-pro])
    (cond
     ((null? ls) '())
     (else
      (cons (per-list (car ls))
	    (f (cdr ls)))))))

(define imp-list
  (let ([ls all-list])
    (map
     (lambda (la)
       (map (lambda (x) (list (list (quotient x 100) (remainder x 100)) (list x))) la))
     ls)))

(define first
  (lambda (one)
    (caar one)))
(define second
  (lambda (one)
    (cadar one)))

(define deal
  (lambda (fst sec)
    (let f ([ls fst])
      (cond
       ((null? ls) '())
       (else
	(let g ([one (car ls)] [se sec])
	  (cond
	   ((null? se) (f (cdr ls)))
	   ((= (second one) (first (car se)))
	    (cons (list (list (first one) (second (car se)))
			(append (cadr one) (cadr (car se))))
		  (g one (cdr se))))
	   (else
	    (g one (cdr se))))))))))
		  
(define new
  (let pro ([init (car imp-list)] [rest (cdr imp-list)] [i 0] [max (length (cdr imp-list))])
  (cond
   ((= i max) init)
   (else
    (append (pro (deal init (list-ref rest i)) (remove (list-ref rest i) rest) 0 (- max 1))
	    (pro init rest (+ i 1) max))))))

(let pro ([no new])
  (cond
   ((null? no) '())
   ((and (= (first (car no)) (second (car no)))
	 (= 6 (length (cadr (car no)))))
    (cons (car no) (pro (cdr no))))
   (else
    (pro (cdr no)))))

#|
(time (begin (define tri ...) ...))
    no collections
    0.000000000s elapsed cpu time
    0.000998556s elapsed real time
    297224 bytes allocated
(((82 82) (8256 5625 2512 1281 8128 2882)))
|#
