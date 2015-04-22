#lang scheme

(require r5rs)

(define (evaluate exp env)
  (if (atom? exp)
      (cond ((symbol? exp)
             (lookup exp env))
            ((or (number? exp) (string? exp) (char? exp) (boolean? exp) (vector? exp)) 
             exp)
            (else (wrong "Cannot evaluate" exp)))
      (case (car exp)
        ((quote)  (cadr exp))
        ((if)     (if (evaluate (cadr exp) env)
                      (evaluate (caddr exp) env)
                      (evaluate (cadddr exp) env)))
        ((begin)  (eprogn (cdr exp) env))
        ((set!)   (update! (cadr exp) env (evaluate (caddr exp) env)))
        ((lambda) (make-function (cadr exp) (cddr exp) env))
        (else     (invoke (evaluate (car exp) env)
                          (evlis (cdr exp) env))))))

(define (eprogn exps env)
  (if (pair? exps)
      (if (pair? (cdr exps))
          (begin (evaluate (car exps) env)
                 (eprogn (cdr exps) env))
          (evaluate (car exps) env))
      empty-begin))

(define empty-begin '())

(define (evlis exps env)
  (if (pair? exps)
      (cons (evaluate (car exps) env)
            (evlis (cdr exps) env))
      '()))

(define (lookup id env)
  (if (pair? env)
      (if (eq? (caar env) id)
          (cdar env)
          (lookup id (cdr env)))
      (wrong "No such binding" id)))

(define (update! id env value)
  (if (pair? env)
      (if (eq? (caar env) id)
          (begin (set-cdr! (car env) value)
                 value)
          (update! id (cdr env) value))
      (wrong "No such binding" id)))

(define (extend env variables values)
  (cond ((pair? variables)
         (if (pair? values)
             (cons (cons (car variables) (car values))
                   (extend env (cdr variables) (cdr values)))
             (wrong "Too less values")))
        ((null? variables)
         (if (null? values)
             env
             (wrong "Too much values")))
        ((symbol? variables) (cons (cons variables values) env))))

(define (invoke fn args)
  (if (procedure? fn)
      (fn args)
      (wrong "Not a function" fn)))

(define (make-function variables body env)
  (lambda (values)
    (eprogn body (extend env variables values))))

(define (atom? exp) (not (pair? exp)))

(define (wrong msg exp) 
  (begin
    (display "Error: ")
    (display msg)
    (newline)
    (display exp)))

(define env.init '())
(define env.global env.init)

(define-syntax definitial
  (syntax-rules ()
    ((definitial name)
     (begin (set! env.global (cons (cons 'name 'void) env.global))
            'name))
    ((definitial name value)
     (begin (set! env.global (cons (cons 'name value) env.global))
            'name))))

(define-syntax defprimitive
  (syntax-rules ()
    ((defprimitive name value arity)
     (definitial name
       (lambda (values)
         (if (= arity (length values))
             (apply value values)
             (wrong "Incorret arity"
                    (list 'name values))))))))

(definitial t #t)
(definitial f #f) ;(definitial f the-false-value)
(definitial nil '())

(definitial foo)
(definitial bar)
(definitial fib)
(definitial fact)

(defprimitive cons cons 2)
(defprimitive car car 1)
(defprimitive set-cdr! set-cdr! 2)
(defprimitive + + 2)
(defprimitive * * 2)
(defprimitive - - 2)
(defprimitive / / 2)
(defprimitive eq? eq? 2)
(defprimitive < < 2)
(defprimitive display display 1)


(define (chapter01-scheme)
  (define (toplevel)
    (begin
      (display "scheme> ")
      (let ((expression (read)))
        (if (or (equal? 'quit expression)
                (equal? 'exit expression))
            (display "Bye!")
            (begin
              (display "=> ")
              (display (evaluate expression env.global))
              (newline)
              (toplevel))))))
  (toplevel))

(chapter01-scheme)