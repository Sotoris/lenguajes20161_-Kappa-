#lang plai

(print-only-errors false)
(require "p4-base.rkt")

;; desugar: FAES -> FAE
;; Convierte una expresion FAES a una expresion FAE sin withs.
(define (desugar expr)
  (type-case FAES expr
    [numS (n) (num n)]
    [binopS (f l r) (binop f (desugar l) (desugar r))]
    [withS (bindings body) (app (fun (map bind-name bindings)
                                     (desugar body))
                                (map desugar
                                     (map bind-val bindings)))]
    [idS (s) (id s)]
    [funS (p b) (fun p (desugar b))]
    [appS (f e) (app (desugar f)
                     (map desugar e))]))

;; interp: FAE -> FAE-Value
;; Evalua una expresion FAE para devolver un FAE-Value.
(define (interp expr env)
  (type-case FAE expr
    [num (n) (numV n)]
    [binop (f l r) (wrap f (interp l env) (interp r env))]
    [id (v) (lookup v env)]
    [fun (bound-id bound-body)
         (closureV bound-id bound-body env)]
    [app (fun-expr args-expr)
         (local([define funV (interp fun-expr env)])
           (if (closureV? funV)
               (let* ([funV-param (closureV-param funV)]
                      [num-params (length funV-param)]
                      [num-args (length args-expr)])
                 (interp (closureV-body funV)
                         (if (= num-params num-args)
                             (multi-param funV-param args-expr env (closureV-env funV))
                             (error 'interp (string-append "Error de aridad: Se esperaban "
                                                           (~a num-params)
                                                           " argumentos y se recibieron "
                                                           (~a num-args))))))
               (error 'interp (string-append (~a funV) " No es una función."))))]))

;; multi-param
;; Agrega una lista de parámetros con sus valores al ambiente dado4.
(define (multi-param params args args-env ac-env)
  (if (empty? params) ac-env
      (multi-param (cdr params) (cdr args) args-env
                   (aSub (car params)
                         (interp (car args) args-env) ac-env))))

; wrap
; Devuelve el FAE-Value del valor de la operacion aritmética dada entre dos
; números numV dados.
(define (wrap f l r)
  (if (and (numV? l) (numV? r))
      (numV (f (numV-n l) (numV-n r)))
      (error 'f (string-append "Los operandos " (~a l) " y " (~a r)
                               " no pueden ser operados por " (~a f)))))

; lookup
; Busca el valor de un identificador en un ambiente dado.
(define(lookup name env)
  (type-case Env env
    [mtSub () (error 'lookup (string-append "El identificador (" (~a name)
                                            ") es libre y no tiene un valor"
                                            " definido para ser evaluado"))]
    [aSub (bound-name bound-value rest-env)
          (if(symbol=? bound-name name) bound-value
             (lookup name rest-env))]))

(define (cparse sexp)
  (desugar (parse sexp)))

(define (rinterp expr)
  (interp expr (mtSub)))


;Pruebas

;desugar
(test (desugar (parse '{+ 3 4})) (binop + (num 3) (num 4)))
(test (desugar (parse '{+ {- 3 4} 7})) (binop + (binop - (num 3) (num 4)) (num 7)))
(test (desugar (parse '{with {{x {+ 5 5}}} x})) (app (fun '(x) (id 'x)) (list (binop + (num 5) (num 5))) ))
(test (desugar (parse '{-{-{+ 4 5} 2}2})) (binop -(binop -(binop + (num 4) (num 5))(num 2))(num 2)))
(test (desugar (parse '{+ 2 {with {{a 3} {b 5}} {* {+ 2 a} b}}}))
      (binop + (num 2) (app (fun '(a b) (binop * (binop + (num 2) (id 'a)) (id 'b)))
                            (list (num 3) (num 5)))))

;interp
(test (rinterp (cparse '3)) (numV 3))
(test (rinterp (cparse '{+ 3 4})) (numV 7))
(test (rinterp (cparse '{+ {- 3 4} 7})) (numV 6))
(test (rinterp (cparse '{with {{x {+ 5 5}}} {+ x x}})) (numV 20))
(test (rinterp (cparse '{with {{x 5}} {+ x x}})) (numV 10))
(test (rinterp (cparse '{with {{x {+ 5 5}}} {with {{y {- x 3}}} {+ y y}}})) (numV 14))
(test (rinterp (cparse '{with {{x 5} {y {- 5 3}}} {+ x y}})) (numV 7))
(test (rinterp (cparse '{with {{x 5}} {+ x {with {{x 3}} 10}}})) (numV 15))
(test (rinterp (cparse '{with {{x 5}} {+ x {with {{x 3}} x}}})) (numV 8))
(test (rinterp (cparse '{with {{x 5}} {+ x {with {{y 3}} x}}})) (numV 10))
(test (rinterp (cparse '{with {{x 5}} {with {{y x}} y}})) (numV 5))
(test (rinterp (cparse '{with {{x 5}} {with {{x x}} x}})) (numV 5))
(test (rinterp (cparse '{{fun {x} x} 3})) (numV 3))
(test (rinterp (cparse '{{{fun {x} x} {fun {x} {+ x 5}}} 3})) (numV 8))
(test (rinterp (cparse '{with {{x 3}} {fun {y} {+ x y}}})) (closureV '(y) (binop + (id 'x) (id 'y)) (aSub 'x (numV 3) (mtSub))))
(test (rinterp (cparse '{with {{x 10}} {{fun {y} {+ y x}} {+ 5 x}}})) (numV 25))

; multi-param
(test (rinterp (cparse '{with {{x 1} {y 2} {z 3}} {+ {+ x y} z}})) (numV 6))
(test (rinterp (cparse '{{fun {x y z} {+ {+ x y} z}} 1 2 3})) (numV 6))
(test (rinterp (cparse '{with {{x 7}{y 8}{z 9}} {+{+{+ z x} y} y}})) (numV 32))
(test (rinterp (cparse '{{fun {x y z}{+{+{* x y}z}y}} 7 4 2}))(numV 34))
(test (rinterp (cparse '{with {{x 2}} {fun {x y} {- x y}}})) (closureV '(x y) (binop - (id 'x) (id 'y)) (aSub 'x (numV 2) (mtSub))))

; pruebas con errores
(test/exn (rinterp (cparse '{{+ 3 2} {* 2 1}}))
          (string-append (~a (numV 5)) " No es una función."))
(test/exn (rinterp (cparse '{{fun {x y} {* x y}} 2 3 4}))
          (string-append "Error de aridad: Se esperaban " (~a 2)
                         " argumentos y se recibieron " (~a 3)))
(test/exn (rinterp (cparse '{* 5 {fun {x} {+ x 3}}}))
          (string-append "Los operandos " (~a (numV 5)) " y "
                         (~a (closureV '(x) (binop + (id 'x) (num 3)) (mtSub)))
                         " no pueden ser operados por " (~a *)))
(test/exn (rinterp (cparse '{with {{x 4}} {+ x y}}))
          (string-append "El identificador (" (~a 'y)
                         ") es libre y no tiene un valor definido para ser evaluado"))

