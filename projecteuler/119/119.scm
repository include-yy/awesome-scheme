(define unique
  (lambda (ls)
    (cond
     ((null? ls) '())
     ((null? (cdr ls)) (cons (car ls) '()))
     ((member (car ls) (cdr ls))
      (cons (car ls) (unique (remove (car ls) ls))))
     (else
      (cons (car ls) (unique (cdr ls)))))))

(define get-num
  (lambda (a)
    (let* ([stra (number->string a)]
	   [clsa (string->list stra)]
	   [lsa (map (lambda (x)
		       (- (char->integer x)
			  (char->integer #\0)))
		     clsa)])
      lsa)))

(define satis?
  (lambda (n)
    (let ([lst (get-num n)])
      (let ([sum (fold-left (lambda (ac x) (+ ac x)) 0 lst)])
	(let f ([n n])
	  (cond
	   ((= sum 1) #f)
	   ((not (zero? (remainder n sum))) #f)
	   ((= n sum) #t)
	   ((> n sum) (f (/ n sum)))
	   (else #f)))))))

(unique (let pro ([n 2] [ls '()])
	  (cond
	   ((= n 16) ls)
	   (else
	    (let ([curr-max (- (expt 10 n) 1)]
		  [curr-min (+ (expt 10 (- n 1)) 1)]
		  [num-max (* n 9)]
		  [num-min 2])
	      (let g ([i num-min] [curr-ls ls])
		(cond
		 ((> i num-max) (pro (+ n 1) curr-ls))
		 (else
		  (let ([k-min (exact (ceiling (log curr-min i)))]
			[k-max (exact (floor (log curr-max i)))])
		    (cond
		     ((> k-min k-max) (g (+ i 1) curr-ls))
		     (else
		      (let ([now-ls (do ([j k-min (+ j 1)] [ls '() (cons (expt i j) ls)])
					((> j k-max) ls))])
			(g (+ i 1) (append (filter satis? now-ls) curr-ls))))))))))))))
#|
(time (unique (let f ...)))
    no collections
    0.000000000s elapsed cpu time
    0.002583500s elapsed real time
    806128 bytes allocated
(248155780267521 72301961339136 20047612231936 6722988818432
 2207984167552 1174711139837 271818611107 68719476736
 24794911296 52523350144 27512614111 10460353203 8303765625
 205962976 612220032 60466176 52521875 17210368 34012224
 1679616 614656 390625 234256 19683 17576 5832 4913 2401 512
 81)
|#
	
