(load "qq.scm")


(define parse
    (lambda (sexp)
        (or
            (parseConst sexp)
            (parser evalVar sexp 'var)
            (parser evalIf sexp 'if3)
            (parseOr sexp)
            (parseLambda sexp)
            ;(parser evalDefine sexp 'def)
            (parser evalDefine sexp 'define)
            (parser evalAssignment sexp 'set)
            (parser evalApplic sexp 'applic)
            (parseBegin sexp)
            )))

(define parseLambda
    (lambda (sexp)
        (or
            (parser eval-variadic-lambda sexp 'lambda-opt)
            ;(parser eval-variadic-lambda sexp 'lambda-var)
            (parser eval-simple-lambda sexp 'lambda-simple)
            (parser eval-optional-args sexp 'lambda-opt)
            )))
            
(define parseConst
    (lambda (sexp)
        (parser evalConst sexp 'const)))
        
(define parseOr
    (lambda (sexp)
        (if (and (list? sexp) (< (length sexp) 3))
            (cadr (evalOr sexp))
            (parser evalOr sexp 'or))))
            
(define parseBegin
    (lambda (sexp)
        (if (and (list? sexp) (= 2 (length sexp)))
            (cadr (evalBegin sexp))
            (parser evalBegin sexp 'seq))))
        
(define flatten 
    (lambda (x)
        (cond ((null? x) '())
                ((pair? x) (append (flatten (car x)) (flatten (cdr x))))
                (else (list x)))))
                
(define parser
   (lambda (evaluator sexp tag) 
     (let* ((lst (evaluator sexp))
            (bool (car lst))
            (val (cdr lst)))
            (if bool `(,tag ,@val) #f)))) 
            
(define *reserved-words*
    '(and begin cond define do else if lambda
    let let* letrec or quasiquote unquote
    unquote-splicing quote set!))
    
(define reserved-word?
    (lambda (word)
        (ormap (lambda (res-word) (equal? word res-word))
                *reserved-words*)))

(define evalConst
    (lambda (c)
        (cond 
            ((quote? c) `(#t ,(cadr c)))
            ((const? c) `(#t ,c))
            ((null? c) `(#t ,c))
            ((vector? c) `(#t ,c))
            ((eq? (void) c) `(#t ,c))
            (else '(#f #f)))))

(define evalVar
    (lambda (v)
        (if 
            (and (not (reserved-word? v)) (symbol? v)) 
            `(#t ,v) 
            '(#f #f))))
        
(define evalIf
    (lambda (s)
        (if (not (and (list? s) (equal? (car s) 'if)))
            '(#f #f)
            (let* 
                ((test (cadr s))
                (dit (caddr s))
                (dif (cdddr s))
                (ret-dif (if (null? dif) (void) (car dif))))
                 `(#t ,(parse test) ,(parse dit) ,(parse ret-dif))))))
                
(define evalOr
    (lambda (s)
        (if (not (and (list? s) (equal? (car s) 'or)))
            '(#f #f)
            (let* 
                ((args (cdr s)))
                (cond 
                    ((= 0 (length args)) `(#t ,(parse #f)))
                    ((= 1 (length args)) `(#t ,(parse (car args))))
                    (else `(#t ,(map parse (cdr s)))))))))
                
(define eval-variadic-lambda
    (lambda (s)
       (if (not (and (list? s) (equal? (car s) 'lambda)))
            '(#f #f)
            (let
                ((args (cadr s))
                (exps (cddr s)))
                (if 
                    (not (symbol? args))
                    '(#f #f)
                    `(#t () args ,(parse `(begin ,@exps))))))))

(define eval-simple-lambda
    (lambda (s)
       (if (not (and (list? s) (equal? (car s) 'lambda)))
            '(#f #f)
            (if (not (list? (cadr s)))
                '(#f #f)
                (let
                    ((args (cadr s))
                    (exps (cddr s)))
                    `(#t ,args ,(parse `(begin ,@exps))))))))

(define eval-optional-args
     (lambda (s)
       (if (not (and (list? s) (equal? (car s) 'lambda)))
            '(#f #f)
            (if (not (pair? (cadr s))) 
                '(#f #f)
                (let*
                    ((args (cadr s))
                    (exps (cddr s))
                    (reversed-list (reverse (flatten args)))
                    (rest (car reversed-list))
                    (proper-args (reverse (cdr reversed-list))))
                    `(#t ,proper-args ,rest ,(parse `(begin ,@exps))))))))
                        
(define evalDefine
    (lambda (s)
        (if (not (and (list? s) (equal? (car s) 'define)))
        '(#f #f)
        (let*
            ((name (cadr s))
            (exps (cddr s)))
            (cond 
                ((symbol? name) `(#t (var ,name) ,(parse (car exps))))
                ((and (pair? name) (not (null? name))) `(#t (var ,(car name)) ,(parse `(lambda ,(cdr name) ,@exps))))
                (else '(#f #f))
                )))))
                
(define evalAssignment
    (lambda (s)
        (if (not (and (list? s) (equal? (car s) 'set!)))
            '(#f #f)
            (let*
                ((name (cadr s))
                (exps (cddr s)))
                (if (symbol? name) 
                    `(#t (var ,name) ,(parse (car exps)))
                    '(#f #f))))))                
                
(define evalApplic
    (lambda (s)
        (if (or (not (list? s)) (reserved-word? (car s)))
            '(#f #f)
            (let*
                ((first (car s))
                (rest (cdr s))
                (parsed-rest (if (null? rest) 
                                '() 
                                (map parse rest))))
                `(#t ,(parse first) ,parsed-rest)))))

(define evalBegin
    (lambda (s)
        (if (not (and (list? s) (equal? (car s) 'begin)))
            '(#f #f)
            (let*
                ((first (cadr s))
                (rest (cddr s))
                (parsed-rest (if (null? rest) 
                                (parse first) 
                                (map parse (cdr s)))))
                `(#t ,parsed-rest)))))
                
                
                
                
                
                
                
                
                
                
                
                
(define parse-2 parse)