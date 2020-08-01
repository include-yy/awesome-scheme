(define outpo1 (open-file-output-port "first.txt" (file-options no-fail) (buffer-mode block) (native-transcoder)))

(let pro1 ([i 0] [out outpo1])
  (cond
   ((= i 127) (close-port out))
   (else
    (let ([po (open-input-file "p059_cipher.txt")])
      (put-datum out i)
      (put-char out #\newline) 
      (let f ([num (get-datum po)])
	(get-char po)
	(cond
	 ((eof-object? num)
	  (close-port po)
	  (put-char out #\newline) (put-char out #\newline) 
	  (pro1 (+ i 1) out))
	 (else
	  (put-char out (integer->char (bitwise-xor i num)))
	  (get-datum po) (get-char po) (get-datum po) (get-char po)
	  (f (get-datum po)))))))))
;;101

(define outpo2 (open-file-output-port "second.txt" (file-options no-fail) (buffer-mode block) (native-transcoder)))

(let pro1 ([i 0] [out outpo2])
  (cond
   ((= i 127) (close-port out))
   (else
    (let ([po (open-input-file "p059_cipher.txt")])
      (put-datum out i)
      (put-char out #\newline) 
      (let f ([num (begin (get-datum po) (get-char po) (get-datum po))])
	(get-char po)
	(cond
	 ((eof-object? num)
	  (close-port po)
	  (put-char out #\newline) (put-char out #\newline) 
	  (pro1 (+ i 1) out))
	 (else
	  (put-char out (integer->char (bitwise-xor i num)))
	  (get-datum po) (get-char po) (get-datum po) (get-char po)
	  (f (get-datum po)))))))))
;;120

(define outpo3 (open-file-output-port "third.txt" (file-options no-fail) (buffer-mode block) (native-transcoder)))

(let pro1 ([i 0] [out outpo3])
  (cond
   ((= i 127) (close-port out))
   (else
    (let ([po (open-input-file "p059_cipher.txt")])
      (put-datum out i)
      (put-char out #\newline) 
      (let f ([num (begin (get-datum po) (get-char po) (get-datum po)(get-char po) (get-datum po))])
	(get-char po)
	(cond
	 ((eof-object? num)
	  (close-port po)
	  (put-char out #\newline) (put-char out #\newline) 
	  (pro1 (+ i 1) out))
	 (else
	  (put-char out (integer->char (bitwise-xor i num)))
	  (get-datum po) (get-char po) (get-datum po) (get-char po)
	  (f (get-datum po)))))))))
;;112

(define three (vector 101 120 112))

(call-with-input-file "p059_cipher.txt"
  (lambda (po)
    (call-with-output-file "final.txt"
      (lambda (out)
	(let f ([chr (get-datum po)] [i 0])
	  (get-char po)
	  (cond
	   ((eof-object? chr))
	   (else
	    (put-char out (integer->char (bitwise-xor chr (vector-ref three (remainder i 3)))))
	    (f (get-datum po) (+ i 1)))))))))
	   

