(library (datatype)
  (export define-datatype cases)
  (import (rnrs lists) (rnrs) (rnrs syntax-case))

  (define-syntax define-datatype
    (lambda (x)
      (define gen-id
        (lambda (template-id . args)
          (datum->syntax
           template-id
           (string->symbol
            (apply string-append
                   (map (lambda (x)
                          (if (string? x)
                              x
                              (symbol->string (syntax->datum x))))
                        args))))))
      (define define-data
        (lambda (x)
          (syntax-case x ()
            [(main-type (variant (field pred) ...))
             (with-syntax
              ([predicator (gen-id #'variant #'variant "?")]
               [struct-len (length #'(field ...))]
               [(pr ...) #'(pred ...)]
               [(selector ...) (map (lambda (x) (gen-id x #'variant "-" x)) #'(field ...))]
               [(index ...) (let f ([ls #'(field ...)] [num 2])
                              (if (null? ls) '() (cons num (f (cdr ls) (+ num 1)))))])
              #'(begin
                  (define variant
                    (lambda (field ...)
                      (vector 'main-type 'variant field ...)))
                  (define predicator
                    (lambda (vec)
                      (let ([selector-ls (list selector ...)]
                            [pred-ls (list pr ...)])
                        (if (not (and (vector? vec)
                                      (= (vector-length vec) (+ struct-len 2))
                                      (eq? (vector-ref vec 0) 'main-type)
                                      (eq? (vector-ref vec 1) 'variant)))
                            #f
                            (let ([var-ls (map (lambda (x) (x vec)) selector-ls)])
                              (for-all (lambda (x y) (x y)) pred-ls var-ls))))))
                  (define selector
                    (lambda (vec)
                      (vector-ref vec index)))
                  ...))])))
      (syntax-case x ()
        [(_ type-name c0 c1 ...)
         (with-syntax
          ([predicator (gen-id #'type-name #'type-name "?")])
          #`(begin
              (define predicator
                (lambda (vec)
                  (and (vector? vec)
                       (eq? (vector-ref vec 0) 'type-name))))
              #,@(let f ([ls #'(c0 c1 ...)])
                   (if (null? ls) '()
                       (cons (define-data #`(type-name #,(car ls)))
                             (f (cdr ls)))))))])))

  (define-syntax cases
    (lambda (x)
      (define gen-id
        (lambda (template-id . args)
          (datum->syntax
           template-id
           (string->symbol
            (apply string-append
                   (map (lambda (x)
                          (if (string? x)
                              x
                              (symbol->string (syntax->datum x))))
                        args))))))
      (syntax-case x ()
        [(_ type-name t c0 c1 ...)
         (with-syntax
          ([type-pred (gen-id #'type-name #'type-name "?")])
          #`(if (not (type-pred t)) #f
                #,(let f ([c0 #'c0]
                          [cmore #'(c1 ...)])
                    (if (null? cmore)
                        (syntax-case c0 (else)
                          [(else e1 e2 ...) #'(begin e1 e2 ...)]
                          [(var (dat ...) e0 e1 ...)
                           (with-syntax
                            ([(selector ...) (map (lambda (x) (gen-id x #'var "-" x)) #'(dat ...))]
                             [pred (gen-id #'var #'var "?")])
                            #'(if (pred t)
                                  (let ([dat (selector t)] ...)
                                    e0 e1 ...)
                                  #f))])
                        (syntax-case c0 ()
                          [(var (dat ...) e0 e1 ...)
                           (with-syntax
                            ([(selector ...) (map (lambda (x) (gen-id x #'var "-" x)) #'(dat ...))]
                             [pred (gen-id #'var #'var "?")])
                            #`(if (pred t)
                                  (let ([dat (selector t)] ...)
                                    e0 e1 ...)
                                  #,(f (car cmore) (cdr cmore))))])))))]))))