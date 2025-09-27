#lang racket/gui

; funcion principal exportada para crear la interfaz de configuracion
(provide crear-pantalla-config)

; listas de opciones disponibles para configurar el juego
(define tamaño-tablero '("8" "9" "10" "11" "12" "13" "14" "15"))
(define opciones-dificultad '("facil" "medio" "dificil"))

(define (crear-pantalla-config parent-container callback-boton)
  
  ; contenedor principal que organiza todos los elementos verticalmente
  (define main-panel (new vertical-panel%
                         [parent parent-container]
                         [alignment '(center center)]
                         [spacing 20]
                         [border 25]))
  
  ; seccion del titulo principal del juego
  (define title-panel (new horizontal-panel%
                          [parent main-panel]
                          [alignment '(center center)]
                          [stretchable-height #f]))
  
  ; etiqueta con el nombre del juego centrada
  (new message%
       [parent title-panel]
       [label "BusCEMinas"]
       [stretchable-height #f])
  
  ; subtitulo explicativo de la pantalla actual
  (new message%
       [parent main-panel]
       [label "Configuración del Tablero"]
       [stretchable-height #f])
  
  ; separador visual entre titulo y controles
  (new message% [parent main-panel] [label ""] [stretchable-height #f])
  
  ; contenedor horizontal para agrupar controles de dimensiones
  (define size-controls (new horizontal-panel%
                            [parent main-panel]
                            [alignment '(center center)]
                            [spacing 40]
                            [stretchable-height #f]))
  
  ; seccion para seleccionar numero de filas del tablero
  (define filas-container (new vertical-panel%
                              [parent size-controls]
                              [alignment '(center center)]
                              [spacing 5]
                              [stretchable-width #f]))
  
  ; etiqueta descriptiva del selector de filas
  (new message% [parent filas-container] [label "Filas"])
  ; menu desplegable con opciones de 8 a 15 filas
  (define celdas-filas (new choice%
                           [parent filas-container]
                           [label ""]
                           [choices tamaño-tablero]
                           [selection 0])) ; seleccion inicial en la primera opcion
  
  ; seccion para seleccionar numero de columnas del tablero
  (define columnas-container (new vertical-panel%
                                 [parent size-controls]
                                 [alignment '(center center)]
                                 [spacing 5]
                                 [stretchable-width #f]))
  
  ; etiqueta descriptiva del selector de columnas
  (new message% [parent columnas-container] [label "Columnas"])
  ; menu desplegable con opciones de 8 a 15 columnas
  (define celdas-columnas (new choice%
                              [parent columnas-container]
                              [label ""]
                              [choices tamaño-tablero]
                              [selection 0])) ; seleccion inicial en la primera opcion
  
  ; separador visual entre controles de tamaño y dificultad
  (new message% [parent main-panel] [label ""] [stretchable-height #f])
  
  ; contenedor horizontal para centrar el selector de dificultad
  (define difficulty-panel (new horizontal-panel%
                               [parent main-panel]
                               [alignment '(center center)]
                               [stretchable-height #f]))
  
  ; contenedor vertical para organizar etiqueta y selector de dificultad
  (define difficulty-container (new vertical-panel%
                                   [parent difficulty-panel]
                                   [alignment '(center center)]
                                   [spacing 5]
                                   [stretchable-width #f]))
  
  ; etiqueta descriptiva del selector de dificultad
  (new message% [parent difficulty-container] [label "Nivel de Dificultad"])
  ; menu desplegable con tres niveles de dificultad
  (define tablero-dificultad (new choice%
                                 [parent difficulty-container]
                                 [label ""]
                                 [choices opciones-dificultad]
                                 [selection 0])) ; seleccion inicial en facil
  
  ; espaciador flexible que empuja el boton hacia la parte inferior
  (new message% [parent main-panel] [label ""] [stretchable-height #f])
  
  ; contenedor horizontal para centrar el boton de inicio
  (define button-panel (new horizontal-panel%
                           [parent main-panel]
                           [alignment '(center center)]
                           [stretchable-height #f]))
  
  ; boton principal que inicia el juego con la configuracion seleccionada
  (define boton-jugar (new button%
                          [parent button-panel]
                          [label "Iniciar Juego"]
                          [min-width 120]
                          [min-height 35]
                          [callback (lambda (b e)
                                    ; obtener indices de las selecciones actuales
                                    (define filas-indice (send celdas-filas get-selection))
                                    (define columnas-indice (send celdas-columnas get-selection))
                                    (define dificultad-indice (send tablero-dificultad get-selection))
                                    
                                    ; convertir indices a valores usando list-ref de racket
                                    (define filas-tamaño (list-ref tamaño-tablero filas-indice))
                                    (define columnas-tamaño (list-ref tamaño-tablero columnas-indice))
                                    (define dificultad (list-ref opciones-dificultad dificultad-indice))
                                    
                                    ; ejecutar callback con los parametros seleccionados
                                    (callback-boton filas-tamaño columnas-tamaño dificultad))]))
  
  ; devolver el panel principal como resultado de la funcion
  main-panel)