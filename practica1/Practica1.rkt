#lang plai

(print-only-errors false)

; Garcia Garcia Raul Eduerado
; Rivera Mercado Sergio
; Rivera Sotomayor Luis Rafael
; EJERCICIOS



; Función que calcula el exponente 
; entre a y b.
(define (pow a b)
  (define (pow-r b result)
    (if (= b 0)
        result
        (pow-r (- b 1) (* result a))))
  (pow-r b 1))


; Función que regresa el promedio de una lista
; de números no vacía.
(define (average list)
  (if (empty? list) 0
      (quotient (reduce + list) (length list))))

; Función que regresa una lista con los números
; primos contenidos entre 2 y a.
(define (primes a)
  (if (< a 2) empty
      (if (hay-divisores a 2)
          (primes (- a 1))
          (mconcat (primes (- a 1)) (cons a empty)))))

; Función que une los elementos de dos listas
; asignandolos ordenadamente por pares.
(define (zip x y)
  (if (or (empty? x) (empty? y)) empty
      (cons (cons (car x) (cons (car y) empty))
            (zip (cdr x) (cdr y)))))

; Funcion que aplica una función de aridad 2 dada
; sobre los elementos de list.
(define (reduce op l)
  (cond
    [(empty? l) (error "No hay parametros suficientes.")]
    [(empty? (cdr l)) (car l)]
    [else (op (car l) (reduce op (cdr l)))]))

; Función que concatena dos listas.
(define (mconcat x y)
  (if (empty? x) y
      (cons (car x) (mconcat (cdr x) y))))

; Función que aplica una función de aridad 1
; a cada elemento de una list.
(define (mmap op l)
  (if (empty? l)
      (display "No hay parámetros que calcular")
      (if (empty? (cdr l))
          (cons (op (car l)) empty)
          (cons (op (car l)) (mmap op (cdr l))))))

; Función que regresa de una lista, la sublista
; cuyos elementos no cumplen con el predicado
; de un argumento dado.
(define (mfilter λ l)
  (if(empty? l) empty
     (if (λ (car l))
         (cons (car l) (mfilter λ (cdr l)))
         (mfilter λ (cdr l)))))

;any?
(define (any?number? lst)
  (cond
[(empty? lst) #f ]
[(empty? (filter number? lst)) #f  ]
[else #t ]
)
  )
(define (any?symbol? lst)
  (cond
[(empty? lst) #f ]
[(empty? (filter number? lst)) #t  ]
[else #f ]
)
  )

(any?symbol? '(a b c))
(any?number? '(a b c))

;every?
(define (every?number? lst)
  (cond
[(empty? lst) #t ]
[(equal? (filter number? lst) lst) #t  ]
[else #f ]
)
  )
(define (every?symbol? lst)
  (cond 
[(empty? lst) #f ]
[(empty? (filter number? lst) ) #t  ]
[else #f ]
)
  )

(symbol? '(a b c))
(every?symbol? '(1 2 3))

;mpowerset
(define (mpowerset lst)
  (if (null? lst)
      '(())
      (append-map (lambda (x)
                    (list x (cons (car lst) x)))
                  (mpowerset (cdr lst)))))
(mpowerset '(1 2 3))

; FUNCIONES AUXILIARES

; Función auxiliar que indica si hay divisores
; de a desde b, con a mayor a b.
(define (hay-divisores a b)
  (if (or (= a b) (< a 2)) #f
      (if (= (modulo a b) 0) #t
          (hay-divisores a (add1 b)))))

;PRUEBAS

; pow
(test (pow 2000 0) 1)
(test (pow 2 3) 8)
(test (pow 8 6) 262144)
; average
(test (average '(5)) 5)
(test (average '(3 2 6 2 1 7 2 1)) 3)
(test (average '(10 7 13)) 10)
(test (average '(5 5 5 10 10)) 7)
(test (average '(10 10 10 10 5)) 9)

; primes
(test (primes 30) '(2 3 5 7 11 13 17 19 23 29))
(test (primes 11) '(2 3 5 7 11))
(test (primes 1) '())
(test (primes 17) '(2 3 5 7 11 13 17))
(test (primes 12) '(2 3 5 7 11))

; zip
(test (zip '(1 2) '(3 4)) '((1 3) (2 4)))
(test (zip '(1 2 3) '()) '())
(test (zip '(8 9) '(3 2 1 4)) '((8 3) (9 2)))
(test (zip '(8 9 1 2) '(3 4)) '((8 3) (9 4)))
(test (zip '(1 3 5) '(2 4 6)) '((1 2) (3 4) (5 6)))

; reduce
(test (reduce + '(1 2 3 4 5 6 7 8 9 10)) 55)
(test (reduce zip '((1 2 3) (4 5 6) (7 8 9)))
      '((1 (4 7)) (2 (5 8)) (3 (6 9))))
(test (reduce * '(1 2 3 4 5)) 120)


; mconcat
(test (mconcat '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))
(test (mconcat '() '(4 5 6)) '(4 5 6))
(test (mconcat '(1 2 3) '()) '(1 2 3))
(test (mconcat '(1 2) '(3 4)) '(1 2 3 4))
(test (mconcat '(1 2 3) '(a b c)) '(1 2 3 a b c))


; mmap
(test (mmap add1 '(1 2 3 4)) '(2 3 4 5))
(test (mmap car '((1 2 3) (4 5 6) (7 8 9))) '(1 4 7))
(test (mmap cdr '((1 2 3) (4 5 6) (7 8 9))) '((2 3) (5 6) (8 9)))
(test (mmap number? '(1 a 2 s 3 d)) '(#t #f #t #f #t #f))
(test (mmap zero? '(1 0 0 1 )) '(#f #t #t #f))


; mfilter
(test (mfilter (lambda (x) (not (zero? x))) '(2 0 1 4 0))
      '(2 1 4))
(test (mfilter (lambda (l) (not (empty? l))) '((1 4 2) () (2 4) ()))
      '((1 4 2) (2 4)))
(test (mfilter (lambda (n) (= (modulo n 2) 0)) '(1 2 3 4 5 6))
      '(2 4 6))
(test (mfilter (lambda (a) (< a 3)) '(1 2 3))
      '(1 2))
(test (mfilter (lambda (m) (empty? (cdr m))) '((a b) (c) (d e) (f)))
      '((c) (f)))

;any?
(test (any?number? '()) #f)
(test (any?number? '(a b c d 1)) #t)
(test (any?symbol? '(1 2 3 4)) #f)

;every?
(test (every?number? '()) #t)
(test (every?number? '(1 2 3)) #t)
(test (every?number? '(1 2 3 a)) #f)
(test (every?symbol? '(1 2 3 a)) #f)

;mpowerset

(test (mpowerset '()) '(()))
(test (mpowerset '(1)) '(() (1)))
(test (mpowerset '(1 2)) '(() (1) (2) (1 2)))
(test (mpowerset '(1 2 3)) '(() (1) (2) (3) (1 2) (1 3) (2 3) (1 2 3)))
