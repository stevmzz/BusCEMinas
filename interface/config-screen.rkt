#lang racket/gui

; funcion para crear la pantalla de configuracion
(provide crear-pantalla-config)
(provide (all-defined-out))

(define filas_escogidas #f)
(define columnas_escogidas #f)
(define dificultad_escogida #f)

;Lista de opciones para tamaño de tablero y dificultad

(define tamaño_tablero '("8" "9" "10" "11" "12" "13" "14" "15"))
(define opciones_dificultad '("facil" "medio" "dificil"))

(define (crear-pantalla-config parent-container callback-boton)

  
  ; panel principal 
  (define config-panel (new horizontal-panel%
                           [parent parent-container]
                           [alignment '(center center)]))

  ; Menu para escoger el tamaño de las filas del tablero
  (define celdas_filas (new choice%
                          [parent config-panel]
                          [label "filas"]
                          [style '(vertical-label)]
                          [choices tamaño_tablero]))
  

  ; Menu para escoger el tamaño de las columnas del tablero
  (define celdas_columnas (new choice%
                          [parent config-panel]
                          [label "columnas"]
                          [style '(vertical-label)] 
                          [choices tamaño_tablero]))
   ; Menu para escoger el tamaño de las columnas del tablero
  (define tablero_dificultad (new choice%
                          [parent config-panel]
                          [label "columnas"]
                          [style '(vertical-label)] 
                          [choices opciones_dificultad]))

  (define (elemento lista index)
    (cond ( (null? lista)#f)
          ((equal? index 0)
           (car lista))
          (else (elemento (cdr lista) (- index 1)))))
  
  ; boton para ir al juego
  (define boton (new button%
                    [parent config-panel]
                    [label "jugar"]
                    [callback (lambda (b e)
                                (define filas_indice (send celdas_filas get-selection))
                                (define columnas_indice (send celdas_columnas get-selection))
                                (define dificultad_indice (send tablero_dificultad get-selection))

                                
                                (define filas_tamaño (elemento tamaño_tablero filas_indice))
                                (define columnas_tamaño (elemento tamaño_tablero columnas_indice))
                                (define dificultad (elemento opciones_dificultad dificultad_indice))

                               
                                (set! filas_escogidas filas_tamaño)
                                (set! columnas_escogidas columnas_tamaño)
                                (set! dificultad_escogida dificultad)
                                (callback-boton))]))



  
  ; retornar el panel
  config-panel)