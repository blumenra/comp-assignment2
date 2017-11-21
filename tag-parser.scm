(load "qq.scm")

(define parse
    (lambda (sexp)
        (or
            (parser evalConst sexp 'const)
            (parser evalVar sexp 'var)
            (parser evalIf sexp 'if3)
            )))

(define parser
   (lambda (evaluator sexp tag) 
     (let* ((lst (evaluator sexp))
            (bool (car lst))
            (val (cdr lst)))
            (if bool `(,tag ,@val) #f)))) 
            
(define *void-object* (if #f #f))

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
            (else '(#f #f)))))
        
(define parseConst
   (lambda (c) 
     (let* ((lst (evalConst c))
            (bool (car lst))
            (val (cadr lst)))
            (if bool `(const ,val) #f))))

(define evalVar
    (lambda (v)
        (if 
            (and (not (reserved-word? v)) (symbol? v)) 
            `(#t ,v) 
            '(#f #f))))
    
(define parseVar
     (lambda (v)
        (let*
            ((lst (evalVar v))
            (bool (car lst))
            (val (cadr lst)))
            (if bool `(var ,val) #f))))
        
(define evalIf
    (lambda (s)
        (if (not (equal? (car s) 'if))
            #f
            (let* 
                ((test (parse (cadr s)))
                (dit (parse (caddr s)))
                (dif (cadddr s))
                (ret-dif (if (null? dif) *void-object* (parse dif))))
                 `(#t ,test ,dit ,ret-dif)))))
                
              
                  
       
        
        
        