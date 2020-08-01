(let pro ([i 1])
  (let f ([j i])
    (let* ((k (- 1000 i j))
	   (cmp (- (+ (* i i) (* j j)) (* k k))))
      (cond
       ((< cmp 0)
	(f (+ j 1)))
       ((= cmp 0)
	(* i j k))
       ((> cmp 0)
	(pro (+ i 1)))))))
#|
(time (let pro ...))
    no collections
    0.000000000s elapsed cpu time
    0.000806304s elapsed real time
    0 bytes allocated
31875000
|#
      
