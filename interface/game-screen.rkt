#lang racket/gui

; importar la logica del tablero
(require "../logic/board-generator.rkt")
(require "../interface/config-screen.rkt")

; funcion para crear la pantalla del juego
(provide crear-pantalla-juego)
(define (crear-pantalla-juego parent-container callback-volver filas columnas dificultad)
(define tamaño_filas (string->number filas))
(define tamaño_columnas (string->number columnas))
; crear el tablero usando la logica
(define mi-tablero (tablero tamaño_filas tamaño_columnas dificultad))
  
; panel principal
(define game-panel (new vertical-panel%
                         [parent parent-container]))

; contenedor de regiones de la ventana
(define container (new vertical-panel%
                       [parent game-panel]))

; parte de arriba de la ventana
(define upper-half (new horizontal-panel%
                        [parent container]
                        (min-width 0)
                        (min-height 0)))

; parte de abajo de la ventana (zona de las celdas)
(define lower-half (new horizontal-panel%
                        [parent container]
                        (min-width 0)
                        (min-height 0)))

; parte superior del juego 
(new canvas% [parent upper-half]
             [paint-callback
              (lambda (canvas dc1)
                (send dc1 set-pen "gray" 0 'transparent)
                (send dc1 set-brush "gray" 'solid)
                (send dc1 draw-rectangle 0 0 800 200)
                (send dc1 set-scale 3 3)
                (send dc1 set-text-foreground "black")
                (send dc1 draw-text "BusCEMinas!" 0 0))])

; funcion auxiliar para generar una secuencia de numeros
(define (rango-lista inicio fin)
  (cond
    [(>= inicio fin) '()]
    [else (cons inicio (rango-lista (+ inicio 1) fin))]))

; funcion para crear botones de una columna
(define (crear-botones-columna col-panel i tamaño_filas mi-tablero)
  (define (crear-boton-fila j)
    (cond
      [(>= j tamaño_filas) '()]
      [else
       (define indice (+ (* i tamaño_filas) j))
       (define celda-datos (elemento mi-tablero indice))
       (define es-mina (mine celda-datos))
       (define num-adyacentes (second celda-datos))
       
       (cons (new button%
                  [horiz-margin 0]
                  [vert-margin 0] 
                  [label ""]
                  [parent col-panel]
                  [min-height 60]
                  [callback (lambda (b event)
                            ; al hacer clic, mostrar los datos reales de la celda
                            (if (= es-mina 1)
                                (send b set-label "MINA")
                                (send b set-label (number->string num-adyacentes))))])
             (crear-boton-fila (+ j 1)))]))
  
  (crear-boton-fila 0))

; funcion para crear todas las columnas
(define (crear-todas-columnas i tamaño_columnas tamaño_filas lower-half mi-tablero)
  (cond
    [(>= i tamaño_columnas) '()]
    [else
     (define col-panel (new vertical-panel% [parent lower-half]))
     (define botones-columna (crear-botones-columna col-panel i tamaño_filas mi-tablero))
     (cons botones-columna 
           (crear-todas-columnas (+ i 1) tamaño_columnas tamaño_filas lower-half mi-tablero))]))

; generador de celdas
(define celdas 
  (crear-todas-columnas 0 tamaño_columnas tamaño_filas lower-half mi-tablero))

; retornar el panel
  game-panel)