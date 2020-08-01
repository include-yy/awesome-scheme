(define e60 (expt 2 60))
(define e30 (expt 2 30))
(define e20 (expt 2 20))
(define e12 (expt 2 12))

(define judge
  (lambda (n)
    (cond
     ((= n 2) #f)
     (else
      (not (or (= 1 (remainder e30 (- n 1)))
	       (= 1 (remainder e20 (- n 1)))
	       (= 1 (remainder e12 (- n 1)))))))))


(let ([ fac-list
  (do ([i 1 (+ i 2)]
       [tot '() (if (zero? (remainder (- e60 1) i))
		    (cons i (cons (/ (- e60 1) i) tot)) tot)])
      ((> i (- e30 1)) tot))])
  (let pro ([ls (map (lambda (x) (+ x 1)) fac-list)] [tot 0])
    (cond
     ((null? ls) tot)
     ((judge (car ls))
      (pro (cdr ls) (+ tot (car ls))))
     (else
      (pro (cdr ls) tot)))))

#|
(time (let ((...)) ...))
    3715 collections
    66.250000000s elapsed cpu time, including 0.062500000s collecting
    66.296721500s elapsed real time, including 0.066138300s collecting
    15693114992 bytes allocated, including 15690648048 bytes reclaimed
3010983666182123972
|#
