(define nump (open-file-input-port "data.txt"
				   (file-options no-create)
				   (buffer-mode block)
				   (native-transcoder)))

(define numvec (make-vector 100 0))

(call-with-port
 nump
 (lambda (po)
   (let f ([i 0] [num (get-datum po)])
     (cond
      ((eof-object? num))
      (else
       (vector-set! numvec i num)
       (f (+ i 1) (get-datum po)))))))

(do ((i 0 (+ i 1)) (sum 0 (+ sum (vector-ref numvec i)))) ((>= i 100) sum))
