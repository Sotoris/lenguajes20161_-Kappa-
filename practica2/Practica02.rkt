#lang plai
;Rivera Sotomayr Luis Rafael
;Rvera Mercado Sergio
;Garcia Garcia Raul Eduardo

;EJERCICIOS


; TIPOS DE DATOS

; Definición de un arreglo dada una longitud y una lista de elementos.
(define-type Array
  [MArray (length number?) (elems list?)])

; Definición de una lista la cual puede ser vacía o ser una lista seguida
; de un elemento.
(define-type MList
  [MEmpty]
  [MCons (head any?) (rest MList?)])

; Definición de un árbol n-ario, el cual puede ser vacío o ser un elemento
; raíz con una lista de subarboles.
(define-type NTree
  [TLEmpty]
  [NodeN (root any?) (sub (listof NTree?))])

; Definición de una posición, la cual puede ser de dos o tres dimensiones
; según el número de coordenadas de los parametros dados.
(define-type Position
  [2D-Point (x number?) (y number?)]
  [3D-Point (x number?) (y number?) (z number?)])

; Definición de una figura de dos dimensiones.
(define-type Figure
  [Circle (center 2D-Point?) (radio number?)]
  [Square (up-left 2D-Point?) (side number?)]
  [Rectangle (up-left 2D-Point?) (width number?) (height number?)])


; FUNCIONES

; Función que devuelve un nuevo arreglo con los valores de un MArray dado,
; o con el valor modificado de la posición y valor dados, si es que la posición
; dada es menor o igual a la longitud definida en el MArray.
(define (setvalue A n v)
  (let ([x (MArray-elems A)] [m (MArray-length A)])
    (cond
      [(empty? x)
       (if (>= n m) (error "Out of bounds.") (MArray m x))]
      [(>= n m)
       (error "Out of bounds.")]
      [else
       (MArray
        m (letrec ([set (λ (a b c)
                          (if (empty? a) a
                              (cons (if (= b 0) v (car a))
                                    (set (cdr a) (sub1 b) c))))])
            (set x n v)))])))

; Función que representa la información de una MList en una cadena con formato imprimible.
(define (printML l)
  (string-append
   "[" (letrec ([aux
                 (λ (x)
                   (type-case MList x
                     [MEmpty () "]"]
                     [MCons (a y) (string-append
                                   (if(MList? a) (printML a) (~a a))
                                   (if(MEmpty? y) "]"
                                      (string-append
                                       ", " (aux y))))]))]) (aux l))))

; Función que concatena los elementos de dos MList dadas.
(define (concatML x y)
  (type-case MList x
    [MEmpty () y]
    [MCons (a b) (MCons a (concatML b y))]))

; Función que calcula la longitud de una MList
(define (lengthML x)
  (type-case MList x
    [MEmpty () 0]
    [MCons (a b) (add1 (lengthML b))]))

; Función que calcula el área de una Figure dada.
(define (area fig)
  (type-case Figure fig
    [Circle (c r) (* pi (* r r))]
    [Square (esq s) (* s s)]
    [Rectangle (esq b h) (* b h)]))

; Función que verifica si un punto en el plano se encuentra
; dentro del área de una Figure dada, devuelve #t si se cumple
; que las coordenadas que delimitan a la figura acotan al punto,
; #f en otro caso.
(define (in-figure? fig p)
  (let ([px (2D-Point-x p)] [py (2D-Point-y p)])
    (type-case Figure fig
      [Circle (c r) (>= r (dist-2D c p))]
      [Square (up-left s) (let* ([x (2D-Point-x up-left)]
                                 [y (2D-Point-y up-left)])
                            (and (<= x px) (<= px (+ x s))
                                 (>= y py) (>= py (- y s))))]
      [Rectangle (up-left b h) (let* ([x (2D-Point-x up-left)]
                                      [y (2D-Point-y up-left)])
                                 (and (<= x px) (<= px (+ x b))
                                      (>= y py) (>= py (- y h))))])))

; FUNCIONES AUXILIARES.

; Predicado que toma verdadero cualquier valor.
(define (any? x) #t)

; Función auxiliar que calcula la distancia entre dos puntos
; dados en el plano.
(define (dist-2D a b)
  (sqrt (+ (expt (- (2D-Point-x b) (2D-Point-x a)) 2)
           (expt (- (2D-Point-y b) (2D-Point-y a)) 2))))

; Función auxiliar que calcula la distancia entre dos puntos
; dados en el espacio.
(define (dist-3D a b)
  (expt (+ (abs (expt (- (3D-Point-x b) (3D-Point-x a)) 3))
           (abs (expt (- (3D-Point-y b) (3D-Point-y a)) 3))
           (abs (expt (- (3D-Point-z b) (3D-Point-z a)) 3))) 1/3))


; DEFINICIÓN DE CONSTANTES PARA REALIZAR LAS PRUEBAS.

(define MA-A (MArray 5 '(1 2 3 4 5)))
(define MA-B (MArray 5 '()))

(define ML-A (MCons "a" (MEmpty)))
(define ML-B (MCons "b" (MCons "c" (MEmpty))))
(define ML-C (MCons (MCons 4 (MEmpty)) (MEmpty)))
(define ML-D (MCons (MCons 5 (MCons (MCons 6 (MEmpty))
                                       (MEmpty)))
                       (MCons (MCons 7 (MEmpty)) (MEmpty))))

(define NT-A (TLEmpty))
(define NT-B (NodeN "a" (list (TLEmpty))))
(define NT-C (NodeN 1 (list (TLEmpty) (TLEmpty))))
(define NT-D (NodeN "b"
                       (list (NodeN 2 (list (TLEmpty)))
                             (NodeN 3 (list (TLEmpty))))))
(define NT-E (NodeN 'Luis
                      (list (NodeN 'Angel
                                   (list (NodeN 'Aguilar
                                                (list (TLEmpty)))
                                         (TLEmpty)))
                            (NodeN "Abc"
                                   (list (TLEmpty))))))

(define P-A (2D-Point 0 0))
(define P-B (2D-Point 0 3))
(define P-C (2D-Point 5 5))
(define P-D (2D-Point 3 3))
(define P-E (2D-Point 2 5))
(define P-F (2D-Point -2 6))

(define C-A (Circle P-A 3))
(define C-B (Circle P-C 4))
(define S-A (Square P-A 10))
(define S-B (Square P-E 3))
(define R-A (Rectangle P-A 4 2))
(define R-B (Rectangle P-F 8 8))



; PRUEBAS

; Array
(test (MArray 4'(1 2 3)) (MArray 4 '(1 2 3)))

;List
(test (MCons 1 (MCons 2 (MCons 3 (MEmpty)))) (MCons 1 (MCons 2 (MCons 3 (MEmpty)))))
(test (MCons 7 (MCons 4 (MCons 10 (MEmpty)))) (MCons 7 (MCons 4 (MCons 10 (MEmpty)))))

;NTree
(test (NodeN 1 (list (TLEmpty) (TLEmpty))) (NodeN 1 (list (TLEmpty) (TLEmpty) (TLEmpty))))
(test ((NodeN 1 (list (NodeN 2 (list (TLEmpty)))
(NodeN 3 (list (TLEmpty)))
(NodeN 4 (list (TLEmpty) (TLEmpty) (TLEmpty)))))) (NodeN
1
(list
(NodeN 2 (list (TLEmpty)))
(NodeN 3 (list (TLEmpty)))
(NodeN 4 (list (TLEmpty) (TLEmpty) (TLEmpty)))))
)

;Position

;Figure
(test(Circle (2D-Point 2 2) 2) (Circle (2D-Point 2 2) 2))
(test(Square (2D-Point 0 3) 3) (Square (2D-Point 0 3) 3))
(test(Rectangle (2D-Point 0 2) 2 3) (Rectangle (2D-Point 0 2) 2 3))


; setvalueA
(test (setvalue MA-A 4 6) (MArray 5 '(1 2 3 4 6)))
(test (setvalue MA-A 0 9) (MArray 5 '(9 2 3 4 5)))
(test/exn (setvalue MA-A 5 2) "Out of bounds")
(test (setvalue MA-B 2 9) (MArray 5 '()))
(test (setvalue MA-A -2 9) (MArray 5 '(1 2 3 4 5)))

; printML
(test (printML (MEmpty)) "[]")
(test (printML (MCons 7 (MEmpty)))"[7]")
(test (printML (MCons 7 (MCons 4 (MEmpty))))"[7, 4]")
(test (printML (MCons (MCons 1 (MCons 2 (MEmpty))) (MCons 3 (MEmpty))))"[[1, 2], 3]")

; concatML

(test (concatML (MCons 7(MCons 4(MEmpty))) (MCons 1 (MEmpty))) (MCons 7 (MCons 4 (MCons 1 (MEmpty)))))
(test (concatML (MCons 7 (MCons 4 (MEmpty))) (MCons 1 (MCons 10 (MEmpty)))) (MCons 7 (MCons 4 (MCons 1 (MCons 10 (MEmpty))))))

; lengthML
(test (lengthML (MEmpty)) 0)
(test (lengthML (MCons 7 (MCons 4 (MEmpty))))2)

; area
(test (area (Circle (2D-Point 5 5) 4)) 50.26548245743669)
(test (area (Square (2D-Point 0 0) 20)) 400)
(test (area (Rectangle (2D-Point 3 4) 5 10)) 50)

; in-figure?
(test (in-figure? (Rectangle (2D-Point 5 5) 4 6) (2D-Point 4 4)) #f)
