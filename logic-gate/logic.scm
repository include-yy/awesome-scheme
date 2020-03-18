;; 't for true, 'f for false
;;basic operation
(define logic?
  (lambda (token)
    (or (eq? token 't)
        (eq? token 'f))))
(define logic-list?
  (lambda (ls)
    (zero? (fold-left (lambda (x y) (+ x (if (logic? y) 0 1))) 0 ls))))

(define bin-AND
  (lambda (b1 b2)
    (cond
     ((not (and (logic? b1) (logic? b2))) #f)
     ((or (eq? b1 'f) (eq? b2 'f)) 'f)
     (else 't))))

(define bin-OR
  (lambda (b1 b2)
    (cond
     ((not (and (logic? b1) (logic? b2))) #f)
     ((or (eq? b1 't) (eq? b2 't)) 't)
     (else 'f))))

(define-syntax AND
  (syntax-rules ()
    [(_) 't]
    [(_ b)
     (cond
      ((logic? b) b)
      (else #f))]
    [(_ b1 b2)
     (bin-AND b1 b2)]
    [(_ b1 b2 b3 ...)
     (cond
      ((not (logic? b1)) #f)
      ((eq? b1 't) (AND b2 b3 ...))
      ((eq? b1 'f) 'f))]))

(define-syntax OR
  (syntax-rules ()
    [(_) 'f]
    [(_ b)
     (cond
      ((logic? b) b)
      (else #f))]
    [(_ b1 b2)
     (bin-OR b1 b2)]
    [(_ b1 b2 b3 ...)
     (cond
      ((not (logic? b1)) #f)
      ((eq? b1 't) 't)
      ((eq? b1 'f) (OR b2 b3 ...)))]))

(define NOT
  (lambda (b)
    (cond
     ((not (logic? b)) #f)
     (else
      (if (eq? b 't) 'f 't)))))

(define XOR
  (lambda (b1 b2)
    (OR (AND b1 (NOT b2)) (AND (NOT b1) b2))))

(define XNOR
  (lambda (b1 b2)
    (NOT (XOR b1 b2))))

;;basic operation end

;;examples

;;semi-add
(define semi-add
  (lambda (A B)
    (let ([S (XOR A B)]
          [C (AND A B)])
      (list C S))))

;;add-add
(define all-add
  (lambda (A B Ci)
    (let ([C (NOT (OR (AND (NOT A) (NOT B))
                      (AND (NOT A) (NOT Ci))
                      (AND (NOT B) (NOT Ci))))]
          [S (NOT (OR (AND A B (NOT Ci))
                      (AND A (NOT B) Ci)
                      (AND (NOT A) B Ci)
                      (AND (NOT A) (NOT B) (NOT Ci))))])
      (list C S))))

(define 4-all-add
  (lambda (lsb1 lsb2)
    (cond
     ((not (and (= (length lsb1) 4) (= (length lsb2) 4))) #f)
     (else
      (let f ([lb1 (reverse lsb1)]
              [lb2 (reverse lsb2)]
              [Ci 'f]
              [res '()])
        (cond
         ((null? lb1)
          (if (eq? Ci 'f) res (cons Ci res)))
         (else
          (let ([now-ls (all-add (car lb1) (car lb2) Ci)])
            (f (cdr lb1) (cdr lb2) (car now-ls) (cons (cadr now-ls) res))))))))))

(define n-all-add
  (lambda (lsb1 lsb2)
    (cond
     ((not (and (logic-list? lsb1) (logic-list? lsb2))) #f)
     (else
      (let ([len1 (length lsb1)] [len2 (length lsb2)])
        (let ([long (if (> len1 len2) lsb1 lsb2)]
              [short (if (<= len1 len2) lsb1 lsb2)])
          (let f ([lb1 short]
                  [lb2 long]
                  [Ci 'f]
                  [res '()])
            (cond
             ((null? lb1)
              (if (eq? Ci 'f) (append (reverse lb2) res)
                  (if (null? lb2) (cons Ci res)
                  (f (list Ci) lb2 'f res))))
             (else
              (let ([now-ls (all-add (car lb1) (car lb2) Ci)])
                (f (cdr lb1) (cdr lb2) (car now-ls) (cons (cadr now-ls) res))))))))))))

;;selector
(define four-one-selector
  (lambda (D0 D1 D2 D3 EN A1 A0)
    (AND EN (OR (AND (NOT A1) (NOT A0) D0)
                (AND (NOT A1) A0 D1)
                (AND A1 (NOT A0) D2)
                (AND A1 A0 D3)))))

(define eight-one-selector
  (lambda (D0 D1 D2 D3 D4 D5 D6 D7 A2 A1 A0)
    (OR (four-one-selector D0 D1 D2 D3 (NOT A2) A1 A0)
        (four-one-selector D4 D5 D6 D7 A2 A1 A0))))

;;encoder
(define ten-four-encoder
  (lambda (b0 b1 b2 b3 b4 b5 b6 b7)
    (let ([F2 (NOT (AND (NOT b4) (NOT b5) (NOT b6) (NOT b7)))]
          [F1 (NOT (AND (NOT b2) (NOT b3) (NOT b6) (NOT b7)))]
          [F0 (NOT (AND (NOT b1) (NOT b3) (NOT b5) (NOT b7)))])
      (list F2 F1 F0))))

;;decoder
(define three-eight-decoder
  (lambda (A B C)
    (let ([F0 (AND (NOT A) (NOT B) (NOT C))]
          [F1 (AND (NOT A) (NOT B) C)]
          [F2 (AND (NOT A) B (NOT C))]
          [F3 (AND (NOT A) B C)]
          [F4 (AND A (NOT B) (NOT C))]
          [F5 (AND A (NOT B) C)]
          [F6 (AND A B (NOT C))]
          [F7 (AND A B C)])
      (list F0 F1 F2 F3 F4 F5 F6 F7))))

(define four-sixteen-decoder
  (lambda (A B C D)
    (let ([a (three-eight-decoder B C D)])
      (append (map (lambda (x) (AND x (NOT A))) a)
              (map (lambda (x) (AND x A)) a)))))

;;compare
(define four-compare
  (lambda (A3 A2 A1 A0 B3 B2 B1 B0)
    (let* ([A<B (OR (AND (NOT A3) B3)
                    (AND (NOT (XOR A3 B3)) (NOT A2) B2)
                    (AND (NOT (XOR A3 B3)) (NOT (XOR A2 B2)) (NOT A1) B1)
                    (AND (NOT (XOR A3 B3)) (NOT (XOR A2 B2)) (NOT (XOR A1 B1)) (NOT A0) B0))]
           [A=B (AND (NOT (XOR A3 B3)) (NOT (XOR A2 B2)) (NOT (XOR A1 B1)) (NOT (XOR A0 B0)))]
           [A>B (NOT (OR A>B A=B))])
      (list A<B A=B A>B)))