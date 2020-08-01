(define tentwelve (expt 10 12))

(let pro ([s1 1]
          [s2 6]
          [s3 35])
  (let* ([x1 s2]
         [x2 s3]
         [x3 (- (* x2 6) x1)]
         [red x3]
         [blue (+ (sqrt (+ 1 (* 8 red red)))
                  (+ (* red 2) 1))])
    (cond
     ((>= (+ red blue) tentwelve)
      (/ blue 2))
     (else
      (pro x1 x2 x3)))))

#|
(time (let pro ...))
    no collections
    0.000000000s elapsed cpu time
    0.000022300s elapsed real time
    1904 bytes allocated
756872327473
|#