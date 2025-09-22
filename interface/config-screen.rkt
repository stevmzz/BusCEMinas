#lang racket/gui

; funcion para crear la pantalla de configuracion
(provide crear-pantalla-config)
(define (crear-pantalla-config parent-container callback-boton)
  
  ; panel principal
  (define config-panel (new vertical-panel%
                           [parent parent-container]
                           [alignment '(center center)]))
  
  ; boton para ir al juego
  (define boton (new button%
                    [parent config-panel]
                    [label "jugar"]
                    [callback (lambda (b e)
                                (callback-boton))]))
  
  ; retornar el panel
  config-panel)