#lang plai

(require "practica3-base.rkt")


;EJERCICIOS.

;HRZ

; Función que regresa una lista de zonas de frecuencia cardiaca, dado
; el ritmo cardiaco de descanso y el máximo ritmo cardiaco de una persona
(define (zones rest max)
  (let* ([range (- max rest)]
         [fifty (* range 0.5)]
         [sixty (* range 0.6)]
         [seventy (* range 0.7)]
         [eighty (* range 0.8)]
         [ninety (* range 0.9)])
    (list (resting rest (- (+ rest fifty) 1))
          (warm-up (+ rest fifty) (- (+ rest sixty) 1))
          (fat-burning (+ rest sixty) (- (+ rest seventy) 1))
          (aerobic (+ rest seventy) (- (+ rest eighty) 1))
          (anaerobic (+ rest eighty) (- (+ rest ninety) 1))
          (maximum (+ rest ninety) max))))


; Función de acceso a un tipo de zona de una lista dada,
; con un símbolo dado correspondiente al nombre de la zona.
(define (get-zone symb z)
  (if (empty? z) (error "La lista está vacía.")
      (cond
        [(eq? 'resting symb) (list-ref z 0)]
        [(eq? 'warm-up symb) (list-ref z 1)]
        [(eq? 'fat-burning symb) (list-ref z 2)]
        [(eq? 'aerobic symb) (list-ref z 3)]
        [(eq? 'anaerobic symb) (list-ref z 4)]
        [(eq? 'maximum symb) (list-ref z 5)]
        [else (error "Zona no definida en la lista.")])))


; Función que regresa una lista de las zonas correspondientes a las frecuencias
; y lista de zonas dadas.
(define (bpm->zone frec z)
  (if (empty? frec) empty
      (let ([x (car frec)])
        (cond
          [(and (<= (resting-low (get-zone 'resting z)) x)
                (<= x (resting-high (list-ref z 0))))
           (cons (list-ref z 0) (bpm->zone (cdr frec) z))]
          [(and (<= (warm-up-low (get-zone 'warm-up z)) x)
                (<= x (warm-up-high (list-ref z 1))))
           (cons (list-ref z 1) (bpm->zone (cdr frec) z))]
          [(and (<= (fat-burning-low (get-zone 'fat-burning z)) x)
                (<= x (fat-burning-high (list-ref z 2))))
           (cons (list-ref z 2) (bpm->zone (cdr frec) z))]
          [(and (<= (aerobic-low (get-zone 'aerobic z)) x)
                (<= x (aerobic-high (list-ref z 3))))
           (cons (list-ref z 3) (bpm->zone (cdr frec) z))]
          [(and (<= (anaerobic-low (get-zone 'anaerobic z)) x)
                (<= x (anaerobic-high (list-ref z 4))))
           (cons (list-ref z 4) (bpm->zone (cdr frec) z))]
          [(and (<= (maximum-low (get-zone 'maximum z)) x)
                (<= x (maximum-high (list-ref z 5))))
           (cons (list-ref z 5) (bpm->zone (cdr frec) z))]
          [else (error "Frecuencia fuera de zonas.")]))))

;BTREE

; Función que calcula el número de nodos internos de un BTree.
(define (ninBT t)
  (type-case BTree t
    [EmptyBT () 0]
    [BNode (c l e r) (if (and (EmptyBT? l) (EmptyBT? r)) 0
                         (+ 1 (+ (ninBT l) (ninBT r))))]))


; Función que calcula el número de nodos hojas de un BTree.
(define (nlBT t)
  (type-case BTree t
    [EmptyBT () 0]
    [BNode (c l e r) (if (and (EmptyBT? l) (EmptyBT? r)) 1
                         (+ (nlBT l) (nlBT r)))]))


; Función que calcula el número total de nodos de una BTree.
(define (nnBT t)
  (type-case BTree t
    [EmptyBT () 0]
    [BNode (c l e r) (if (and (EmptyBT? l) (EmptyBT? r)) 1
                         (+ 1 (+ (nnBT l) (nnBT r))))]))

; Función que devuelve a partir de un BTree y una función de paridad uno,
; un nuevo BTree con los mismos elementos pero con la función aplicada
; a cada uno de ellos.
(define (mapBT f t)
  (type-case BTree t
    [EmptyBT () (EmptyBT)]
    [BNode (c l e r) (BNode c (mapBT f l) (f e) (mapBT f r))]))


; Función que regresa una lista con los elementos de un BTree recorrido en preorder.
(define (preorderBT t)  
  (type-case BTree t
    [EmptyBT () '()]
    [BNode (c l e r) (append (list e)
                             (preorderBT l)
                             (preorderBT r))]))
; Función que regresa una lista con los elementos de un BTree recorrido en inorder.
(define (inorderBT t)
  (type-case BTree t
    [EmptyBT () '()]
    [BNode (c l e r) (append (inorderBT l)
                             (list e)
                             (inorderBT r))]))

; Función que regresa una lista con los elementos de un BTree recorrido en posorder.
(define (posorderBT t)
  (type-case BTree t
    [EmptyBT () '()]
    [BNode (c l e r) (append (posorderBT l)
                             (posorderBT r)
                             (list e))]))


; Definiciones auxiliares para las pruebas.
(define my-zones (zones 50 180))
(define mrest (get-zone 'resting my-zones))
(define mwarm (get-zone 'warm-up my-zones))
(define mfat (get-zone 'fat-burning my-zones))
(define maero (get-zone 'aerobic my-zones))
(define manae (get-zone 'anaerobic my-zones))
(define mmax (get-zone 'maximum my-zones))

(define mtree (bnn (bnn (bnn ebt 6 ebt) 7 (bnn ebt 8 ebt)) 9
                    (bnn (bnn ebt 10 ebt) 11 (bnn ebt 12 ebt))))

; PRUEBAS.

; zones
(test (zones 50 180) (list
                      (resting 50 114.0)
                      (warm-up 115.0 127.0)
                      (fat-burning 128.0 140.0)
                      (aerobic 141.0 153.0)
                      (anaerobic 154.0 166.0)
                      (maximum 167.0 180)))
(test (zones 60 150) (list
                      (resting 60 104.0)
                      (warm-up 105.0 113.0)
                      (fat-burning 114.0 122.0)
                      (aerobic 123.0 131.0)
                      (anaerobic 132.0 140.0)
                      (maximum 141.0 150)))
(test (zones 70 170) (list
                      (resting 70 119.0)
                      (warm-up 120.0 129.0)
                      (fat-burning 130.0 139.0)
                      (aerobic 140.0 149.0)
                      (anaerobic 150.0 159.0)
                      (maximum 160.0 170)))
(test (zones 55 175) (list
                      (resting 55 114.0)
                      (warm-up 115.0 126.0)
                      (fat-burning 127.0 138.0)
                      (aerobic 139.0 150.0)
                      (anaerobic 151.0 162.0)
                      (maximum 163.0 175)))
(test (zones 65 145) (list
                      (resting 65 104.0)
                      (warm-up 105.0 112.0)
                      (fat-burning 113.0 120.0)
                      (aerobic 121.0 128.0)
                      (anaerobic 129.0 136.0)
                      (maximum 137.0 145)))

; get-zones
(test (get-zone 'anaerobic my-zones) (anaerobic 154.0 166.0))
(test (get-zone 'maximum my-zones) (maximum 167.0 180))
(test/exn (get-zone 'warm-up empty) "La lista está vacía.")
(test/exn (get-zone 'warm-burning my-zones)
          "Zona no definida en la lista.")
(test (get-zone 'resting my-zones) (resting 50 114.0))

; bmp->zone
(test (bpm->zone empty my-zones) '())
(test (bpm->zone '(50 60) my-zones)
      (list (resting 50 114.0) (resting 50 114.0)))
(test (bpm->zone '(140 141) my-zones)
      (list (fat-burning 128.0 140.0) (aerobic 141.0 153.0)))
(test/exn (bpm->zone '(60 70 90) empty) "La lista está vacía.")
(test/exn (bpm->zone '(45 70 180) my-zones)
          "Frecuencia fuera de zonas.")

; ninBT
(test (ninBT (EmptyBT)) 0)
(test (ninBT (BNode < (BNode < (EmptyBT) 3 (EmptyBT)) 1
                    (BNode < (EmptyBT) 2 (EmptyBT)))) 1)
(test (ninBT arb4) 7)
(test (ninBT mtree) 3)
(test (ninBT arb2s) 1)

; nlBT
(test (nlBT (EmptyBT)) 0)
(test (nlBT (BNode < (BNode < (EmptyBT) 3 (EmptyBT)) 1
                   (BNode < (EmptyBT) 2 (EmptyBT)))) 2)
(test (nlBT arb3) 4)
(test (nlBT arbN) 4)
(test (nlBT (bnn arb3 15 mtree)) 8)

; nnBT
(test (nnBT (EmptyBT)) 0)
(test (nnBT (BNode < (BNode < (EmptyBT) 3 (EmptyBT)) 1
                   (BNode < (EmptyBT) 2 (EmptyBT)))) 3)
(test (nnBT arb1s) 1)
(test (nnBT arbol-base) 9)
(test (nnBT maxiarbs) 17)


; mapBT
(test (mapBT add1 (EmptyBT)) (EmptyBT))
(test (mapBT add1 (BNode < (EmptyBT) 1
                         (BNode < (EmptyBT) 2 (EmptyBT))))
      (BNode < (EmptyBT) 2
             (BNode < (EmptyBT) 3 (EmptyBT))))
(test (mapBT (lambda (x) (* x x))
             (BNode < (EmptyBT) 3 (BNode < (EmptyBT) 2 (EmptyBT))))
      (BNode < (EmptyBT) 9 (BNode < (EmptyBT) 4 (EmptyBT))))
(test (mapBT sub1 mtree)
      (BNode < (BNode < (BNode < (EmptyBT) 5 (EmptyBT))
                      6 (BNode < (EmptyBT) 7 (EmptyBT)))
             8 (BNode < (BNode < (EmptyBT) 9 (EmptyBT))
                      10 (BNode < (EmptyBT) 11 (EmptyBT)))))
(test (mapBT string-upcase arb2s)
      (BNode string<? (BNode string<? (EmptyBT) "A" (EmptyBT))
             "B"
             (BNode string<? (EmptyBT) "A" (EmptyBT))))

; preorder
(test (preorderBT (EmptyBT)) '())
(test (preorderBT arbol-base)
      '("F" "B" "A" "D" "C" "E" "G" "I" "H"))
(test (preorderBT arb2s)
      '("b" "a" "a"))
(test (preorderBT mtree)
      '(9 7 6 8 11 10 12))
(test (preorderBT arbNs)
      '("cloj" "que" "adios" "suc" "lambda" "ext" "pow" "trial"))

; inorder
(test (inorderBT (EmptyBT)) '())
(test (inorderBT arbol-base)
      '("A" "B" "C" "D" "E" "F" "G" "H" "I"))
(test (inorderBT arb2s)
      '("a" "b" "a"))
(test (inorderBT mtree)
      '(6 7 8 9 10 11 12))
(test (inorderBT arbNs)
      '("adios" "que" "suc" "cloj" "pow" "ext" "trial" "lambda"))

; posorder
(test (posorderBT (EmptyBT)) '())
(test (posorderBT arbol-base)
      '("A" "C" "E" "D" "B" "H" "I" "G" "F"))
(test (posorderBT arb2s)
      '("a" "a" "b"))
(test (posorderBT mtree)
      '(6 8 7 10 12 11 9))
(test (posorderBT arbNs)
      '("adios" "suc" "que" "pow" "trial" "ext" "lambda" "cloj"))