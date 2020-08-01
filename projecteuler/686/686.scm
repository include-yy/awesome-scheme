(define n30 (expt 10 30))

(let pro ([i 0]
          [curr-val 1]
          [nth 0])
  (cond
   ((= nth 90) '())
   (else
    (let ([now-str (number->string curr-val)])
      (cond
       ((< (string-length now-str) 3)
        (pro (+ i 1) (* curr-val 2)  nth))
       ((and (char=? (string-ref now-str 0) #\1)
             (char=? (string-ref now-str 1) #\2)
             (char=? (string-ref now-str 2) #\3))
        (cons i (pro (+ i 1) (* curr-val 2) (+ nth 1))))
       (else
        (pro (+ i 1) (* curr-val 2) nth)))))))

#|
(time (let pro ...))
    579 collections
    9.750000000s elapsed cpu time, including 0.015625000s collecting
    9.757218300s elapsed real time, including 0.016198300s collecting
    2447200744 bytes allocated, including 2447975880 bytes reclaimed
(90 379 575 864 1060 1545 1741 2030 2226 2515 2711 3000 3196
 3681 3877 4166 4362 4651 4847 5136 5332 5817 6013 6302 6498
 6787 6983 7272 7468 7953 8438 8634 8923 9119 9408 9604 10089
 10574 10770 11059 11255 11544 11740 12225 12710 12906 13195
 13391 13680 13876 14361 14846 15042 15331 15527 15816 16012
 16301 16497 16982 17178 17467 17663 17952 18148 18437 18633
 19118 19314 19603 19799 20088 20284 20573 20769 21254 21450
 21739 21935 22224 22420 22709 22905 23390 23875 24071 24360
 24556 24845 25041)
|#

;;too slow