(define upper (expt 10 15))

(let pro ([i 91]
          [curr-val (expt 2 91)]
          [nth 1])
  (cond
   ((= nth 678910) (- i 2))
   (else
    (let* ([now-str (number->string curr-val)]
           [next-val (* curr-val 2)]
           [three (string->number (substring now-str 0 3))])
      (cond
       ((= three 123)
        (pro (+ i 2) (* next-val 2) (+ nth 1)))
       ((char=? (string-ref now-str 0) #\1)
        (pro (+ i 2) (* next-val 2) nth))
       (else
        (cond
         ((> next-val upper)
          (pro (+ i 1) (quotient next-val 10) nth))
         (else
          (pro (+ i 1) next-val nth)))))))))

#|
(time (let pro ...))
    18395 collections
    86.781250000s elapsed cpu time, including 0.265625000s collecting
    86.805460600s elapsed real time, including 0.330105700s collecting
    77434694384 bytes allocated, including 77436087624 bytes reclaimed
193060223
|#