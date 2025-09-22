#lang racket/gui

; importar la logica del tablero
(require "../logic/board-generator.rkt")

; funcion para crear la pantalla del juego
(provide crear-pantalla-juego)
(define (crear-pantalla-juego parent-container callback-volver)

; crear el tablero usando la logica
(define mi-tablero (tablero 5 5 2))

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
                (send dc1 set-scale 3 3)
                (send dc1 set-text-foreground "blue")
                (send dc1 draw-text "BusCEMinas!" 0 0))])

; generador de celdas
(define celdas 
  (for ([i (in-range 5)])
    (define col-panel (new vertical-panel% [parent lower-half]))
    (for ([j (in-range 5)])
      ; obtener la celda correspondiente del tablero
      (define indice (+ (* i 5) j))
      (define celda-datos (list-ref mi-tablero indice))
      (define es-mina (mine celda-datos))
      (define num-adyacentes (second celda-datos))
      
      (new button%
           [horiz-margin 0]
           [label (format "celda~a" i)]
           [parent col-panel]
           [callback (lambda (b event)
                       ; al hacer clic, mostrar los datos reales de la celda
                       (if (= es-mina 1)
                           (send b set-label "MINA")
                           (send b set-label (number->string num-adyacentes))))]))))

; retornar el panel
  game-panel)