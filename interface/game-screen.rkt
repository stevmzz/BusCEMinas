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
(define total-minas (contar-minas-tablero mi-tablero))
  
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

; Panel para título y contador
(define info-panel (new vertical-panel% [parent upper-half]))

; parte superior del juego 
(new canvas% [parent info-panel]
             [min-height 100]
             [paint-callback
              (lambda (canvas dc1)
                (send dc1 set-pen "gray" 0 'transparent)
                (send dc1 set-brush "gray" 'solid)
                (send dc1 draw-rectangle 0 0 800 100)
                (send dc1 set-scale 2 2)
                (send dc1 set-text-foreground "black")
                (send dc1 draw-text "BusCEMinas!" 10 10))])

; Contador de minas
(define contador-minas-label (new message%
                                  [parent info-panel]
                                  [label "Minas: 0"]
                                  [min-width 120]))

; Botón para volver al menú
(define boton-menu (new button%
                        [parent info-panel]
                        [label "Volver al Menú"]
                        [callback (lambda (b e)
                                    (callback-volver))]))                                  

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
                            (define label-actual (send b get-label))
                            (cond
                              [(equal? label-actual "F") ; si tiene bandera, quitarla
                              (send b set-label "")
                              (actualizar-contador-minas total-minas celdas contador-minas-label)]
                              [(equal? label-actual "") ; si la celda está vacía, mostrar opciones
                              (define respuesta 
                                (message-box/custom "Seleccionar Acción" 
                                                  "¿Qué deseas hacer con esta celda?"
                                                  "Descubrir" 
                                                  "Bandera" 
                                                  #f))
                              (cond
                                [(equal? respuesta 1) ; si el usuario eligió descubrir
                                  (if (= es-mina 1)
                                      (begin
                                        (send b set-label "MINA")
                                        (message-box "¡GAME OVER!" "¡Encontraste una mina!" #f '(ok)))
                                      (begin
                                        (send b set-label (number->string num-adyacentes))
                                        ; si tiene 0 minas adyacentes, descubrir automaticamente vecinos
                                        (when (= num-adyacentes 0)
                                          (auto-descubrir-vecinos indice celdas))))]
                                [(equal? respuesta 2) ; si el usuario eligió bandera
                                  (send b set-label "F")
                                  (actualizar-contador-minas total-minas celdas contador-minas-label)])]
                              [else ; si ya está descubierta, no hacer nada
                              (void)]))])
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

; funcion para obtener un botón especifico de la matriz de botones
(define (obtener-boton celdas indice tamaño_filas)
  (define fila (quotient indice tamaño_filas))
  (define col (remainder indice tamaño_filas))
  (define lista-columna (elemento celdas fila))
  (elemento lista-columna col))

; funcion recursiva para descubrir automaticamente celdas vecinas
(define (auto-descubrir-vecinos indice-inicial celdas)
  (define fila (quotient indice-inicial tamaño_columnas))
  (define col (remainder indice-inicial tamaño_filas))
  
  ; obtener coordenadas de vecinos usando la funcion existente
  (define vecinos-coords (vecinos fila col tamaño_filas tamaño_columnas))
  
  ; funcion auxiliar para procesar cada vecino
  (define (procesar-vecino vecino-coord)
    (define v-fila (first vecino-coord))
    (define v-col (second vecino-coord))
    (define v-indice (+ (* v-fila tamaño_filas) v-col))
    (define v-boton (obtener-boton celdas v-indice tamaño_filas))
    (define v-label (send v-boton get-label))
    
    ; si no está descubierto, descubrirlo
    (when (equal? v-label "")
      (define v-celda (elemento mi-tablero v-indice))
      (define v-es-mina (mine v-celda))
      (define v-adyacentes (second v-celda))
      
      ; solo descubrir si no es mina
      (when (= v-es-mina 0)
        (send v-boton set-label (number->string v-adyacentes))
        
        ; si tambien tiene 0 adyacentes, continuar recursion
        (when (= v-adyacentes 0)
          (auto-descubrir-vecinos v-indice celdas)))))
  
  ; procesar todos los vecinos
  (define (procesar-lista-vecinos lista-vecinos)
    (cond
      [(null? lista-vecinos) (void)]
      [else
       (procesar-vecino (car lista-vecinos))
       (procesar-lista-vecinos (cdr lista-vecinos))]))
  
  (procesar-lista-vecinos vecinos-coords))

; Función para contar banderas actuales en el tablero
(define (contar-banderas-actuales celdas-matriz)
  (define (contar-banderas-columna lista-botones)
    (cond
      [(null? lista-botones) 0]
      [else
       (define boton (car lista-botones))
       (define label (send boton get-label))
       (+ (if (equal? label "F") 1 0)
          (contar-banderas-columna (cdr lista-botones)))]))
  
  (define (contar-todas-columnas lista-columnas)
    (cond
      [(null? lista-columnas) 0]
      [else
       (+ (contar-banderas-columna (car lista-columnas))
          (contar-todas-columnas (cdr lista-columnas)))]))
  
  (contar-todas-columnas celdas-matriz))

; Función para actualizar el contador de minas
(define (actualizar-contador-minas total-minas celdas-matriz label-contador)
  (define banderas-actuales (contar-banderas-actuales celdas-matriz))
  (define minas-restantes (- total-minas banderas-actuales))
  (send label-contador set-label (format "Minas: ~a" minas-restantes)))

(actualizar-contador-minas total-minas celdas contador-minas-label)

; retornar el panel
  game-panel)