(define zero-nine
  (list 0 1 2 3 4 5 6 7 8 9))
(define million 1000000)

(define fac
  (lambda (n)
    (cond
     ((= n 0) 1)
     ((= n 1) 1)
     (else
      (* n (fac (- n 1)))))))

(time(let pro ([i 9] [org-ls zero-nine] [new-ls '()] [accu 0])
  (cond
   ((null? org-ls) (reverse new-ls))
   (else
    (let f ([ls org-ls] [head (car org-ls)] [acc accu])
      (cond
       ((> acc million) (pro (- i 1) (remove head org-ls) (cons head new-ls) (- acc (fac i))))
       ((< acc million) (f (cdr ls) (car ls) (+ acc (fac i))))
       ((= acc million) (pro (- i 1) (remove head org-ls) (cons head new-ls) (- acc (fac i))))))))))
#|
(time (let pro ...))
    no collections
    0.000000000s elapsed cpu time
    0.000002664s elapsed real time
    368 bytes allocated
(2 7 8 3 9 1 5 4 6 0)
|#
