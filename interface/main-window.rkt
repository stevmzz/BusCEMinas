#lang racket/gui

; importar la logica del tablero
(require "../logic/board-generator.rkt")

;Variables
(define ancho 300)
(define largo 300)

; crear el tablero usando la logica
(define mi-tablero (tablero 5 5 8))

#|Ventana|#
; Titulo
(define frame (new frame%
                   [label "BusCEMinas"]
                   [width ancho]
                   [height largo]))

;Contenedor de regiones de la ventana
(define container (new vertical-panel%
                       [parent frame]))

;Parte de arriba de la ventana
(define upper-half (new horizontal-panel%
                        [parent container]
                        (min-width 300)
                        (min-height 50)))

;Parte de abajo de la ventana (Zona de las celdas)
(define lower-half (new horizontal-panel%
                        [parent container]
                        (min-width 300)
                        (min-height 200)))

; Parte superior del juego 
(new canvas% [parent upper-half]
             [paint-callback
              (lambda (canvas dc1)
                ;(send dc1 set-canvas-background "green")
                (send dc1 set-scale 3 3)
                (send dc1 set-text-foreground "blue")
                (send dc1 draw-text "BusCEMinas!" 0 0))])


;Generador de celdas

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
                       ; Aa hacer clic, mostrar los datos reales de la celda
                       (if (= es-mina 1)
                           (send b set-label "MINA")
                           (send b set-label (number->string num-adyacentes))))]))))

(send frame show #t)