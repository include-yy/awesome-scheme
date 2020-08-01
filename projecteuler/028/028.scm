(define left-right
  (lambda (n)
    (+ (* n n) (- n) 1)))
(define right-left-1
  (lambda (n)
    (+ (* 4 n n) (* 4 n) 1)))
(define right-left-2
  (lambda (n)
    (+ (* 4 n n) 1)))

(+ (let f ([i 1] [sum 0])
     (cond
      ((> i 1001) sum)
      (else
       (f (+ i 1) (+ sum (left-right i))))))
   (let f ([i 1] [sum 0])
     (cond
      ((> i 500) sum)
      (else
       (f (+ i 1) (+ sum (right-left-1 i))))))
   (let f ([i 1] [sum 0])
     (cond
      ((> i 500) sum)
      (else
       (f (+ i 1) (+ sum (right-left-2 i)))))))
