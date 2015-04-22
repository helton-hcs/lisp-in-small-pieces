#lang scheme

(require r5rs)

;-------------------------
; Evaluator
;-------------------------
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


(define (repl)
  (define (toplevel)
    (begin
      (display "scheme> ")
      (let ((expression (read)))
        (if (or (equal? 'quit expression)
                (equal? 'exit expression))
            (display "Exiting...")
            (let ((result (evaluate expression env.global)))
              (display "=> ")
              (displayln result)
              (toplevel))))))
  (toplevel))

;-------------------------
; Exercise 1.1
;-------------------------
(define (invoke-trace fn args)
  (define (procedure-name proc env)
    (if (pair? env)
        (if (eq? (cdar env) proc)
            (caar env)
            (procedure-name proc (cdr env)))
        (wrong "No procedure found of " proc)))
  (if (procedure? fn)
      (begin
        (display "[Trace] Calling..: ")
        (display (cons (procedure-name fn env.global) args))
        (newline)
        (let ((result (fn args)))
          (display "[Trace] Returning: ")
          (display (cons (procedure-name fn env.global) args))
          (display " = ")
          (display result)        
          (newline)
          result))
      (wrong "Not a function" fn)))

; Pluging in...
(define old-invoke invoke)
(set! invoke invoke-trace)

; Undo
(set! invoke old-invoke)

;-------------------------
; Exercise 1.2
;-------------------------
(define (evlis-no-recursive-to-1-expression exps env)
  (define (evlis-no-recursive-to-1-expression exps)
    (if (pair? (cdr exps))
        (cons (evaluate (car exps) env)
              (evlis-no-recursive-to-1-expression (cdr exps)))
        (list (evaluate (car exps) env))))
  (if (pair? exps)
      (evlis-no-recursive-to-1-expression exps)
      '()))
 
; Pluging in...
(define old-evlis evlis)
(set! evlis evlis-no-recursive-to-1-expression)

; Undo
(set! evlis old-evlis)

;-------------------------
; Exercise 1.3 (working on...)
;-------------------------
(define (new-lookup id env)
  (if (eq? (caar env) id) 
      (cdar env)
      (new-lookup id (cdr env))))

(define (new-update! id env value)
  (if (eq? (caar env) id)
      (begin (set-cdr! (car env) value)
             value)
      (new-update! id (cdr env) value)))
  
(define (new-extend env names values)
  (cons (cons names values) env))

; Pluging in...
(define old-lookup lookup)
(set! lookup new-lookup)
(define old-update! update!)
(set! update! new-update!)
(define old-extend extend)
(set! extend new-extend)

; Undo
(set! lookup old-lookup)
(set! update! old-update!)
(set! extend old-extend)

;-------------------------
; Running the repl
;-------------------------
(repl)

; Some interesting tests to do...
;(set! fact (lambda (n) (if (eq? n 1) 1 (* n (fact (- n 1))))))
;(fact 5)
;(set! fib (lambda (n) (if (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2))))))
;(fib 5)