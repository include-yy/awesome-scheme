(define nameport (open-input-file "p022_names.txt"))

(define strlst
  (call-with-port
   nameport
   (lambda (po)
     (let loop ([str (get-datum po)] [lst '()])
       (cond
	((eof-object? str) lst)
	(else
	 (get-char po)
	 (loop (get-datum po) (cons str lst))))))))

(define order-ls (list-sort string<? strlst))

(let pro ([index 1] [ls order-ls] [sum 0])
  (cond
   ((null? ls) sum)
   (else
    (let ([new-ls (string->list (car ls))])
      (pro (+ index 1) (cdr ls)
	   (+ sum (* index (apply + (map
				     (lambda (x)
				       (- (char->integer x) (char->integer #\A) -1))
				     new-ls)))))))))
