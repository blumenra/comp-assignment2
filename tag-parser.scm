(load "qq.scm")

(define parse
    (lambda (sexp)
        (or
            (parseConst sexp)
            (parser evalVar sexp 'var)
            (parser evalIf sexp 'if3)
            (parser evalOr sexp 'or)
            (parseLambda sexp)
            )))

(define parseLambda
    (lambda (sexp)
        (or
            (parser eval-variadic-lambda sexp 'lambda-opt)
            ;(parser eval-simple-lambda sexp 'lambda-simple)
            ;(parser eval-optional-args sexp 'lambda-opt)
            )))
            
(define parseConst
    (lambda (sexp)
        (parser evalConst sexp 'const)))
    
            
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
            (if (= 1 (length s)) 
                `(#t ,(parse #f))
                `(#t ,(map parse (cdr s)))))))
                
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
        
        
        