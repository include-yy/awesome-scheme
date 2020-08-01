(let prime-factor ([i 3] [num 600851475143])
  (cond
   ((= i num) num)
   ((= 0 (remainder num i))
    (prime-factor i (/ num i)))
   (else
    (prime-factor (+ i 2) num))))
#|
(time (let prime-factor ...))
    no collections
    0.000000000s elapsed cpu time
    0.000052836s elapsed real time
    16 bytes allocated
6857
|#
