#lang racket

(provide (all-defined-out))

; Crear tablero vacío
(define (crear-tablero filas columnas)
  (make-vector (* filas columnas) (list 0 0)))

; Colocar minas aleatorias
(define (colocar-minas tablero filas columnas num-minas)
  (let loop ((n num-minas))
    (if (= n 0)
        tablero
        (let* ([pos (random (* filas columnas))]
               [celda (vector-ref tablero pos)])
          (if (= (first celda) 1) ; ya hay mina en esa celda
              (loop n)
              (begin
                (vector-set! tablero pos (list 1 0))
                (loop (sub1 n))))))))

; Vecinos válidos de una celda
(define (vecinos i j filas columnas)
  (define desplazamientos '((-1 -1) (-1 0) (-1 1)
                            (0 -1)           (0 1)
                            (1 -1)  (1 0)  (1 1)))
  (for/list ([d desplazamientos]
             #:when (and (<= 0 (+ i (first d)) (sub1 filas))
                         (<= 0 (+ j (second d)) (sub1 columnas))))
    (list (+ i (first d)) (+ j (second d)))))

; Calcular adyacentes
(define (calcular-adyacentes tablero filas columnas)
  (for* ([i (in-range filas)]
         [j (in-range columnas)])
    (let* ([idx (+ (* i columnas) j)]
           [celda (vector-ref tablero idx)])
      (unless (= (first celda) 1) ; si no es mina
        (define count
          (for/sum ([v (vecinos i j filas columnas)])
            (let* ([vi (first v)]
                   [vj (second v)]
                   [pos (+ (* vi columnas) vj)])
              (if (= (first (vector-ref tablero pos)) 1) 1 0))))
        (vector-set! tablero idx (list 0 count)))))
  tablero)

; Función tablero principal
(define (tablero filas columnas nivel)
  (define total (* filas columnas))
  (define porcentaje
    (case nivel
      [(facil fácil easy) 0.10]
      [(medio medium) 0.15]
      [(dificil difícil hard) 0.20]
      [else (error "Nivel no reconocido: use 'facil, 'medio o 'dificil")]))
  (define num-minas (max 1 (round (* total porcentaje))))
  (let ([t (crear-tablero filas columnas)])
    (colocar-minas t filas columnas num-minas)
    (calcular-adyacentes t filas columnas)
    (vector->list t)))

; Auxiliar para saber si hay mina
(define (mine celda)
  (first celda))