#lang racket/gui

; importar pantallas
(require "config-screen.rkt")
(require "game-screen.rkt")

; ventana principal
(define frame (new frame%
                   [label "BusCEMinas"]
                   [width 350]
                   [height 430]))

; contenedor para las pantallas
(define container (new panel%
                      [parent frame]))

; funcion para limpiar contenedor
(define (limpiar-contenedor)
  (for-each (lambda (child)
              (send child show #f))
            (send container get-children)))

; funcion para ir al juego
(define (ir-a-juego)
  (limpiar-contenedor)
  (crear-pantalla-juego container volver-a-config))

; funcion para volver a configuración
(define (volver-a-config)
  (limpiar-contenedor)
  (crear-pantalla-config container ir-a-juego))

; iniciar con pantalla de configuracion
(crear-pantalla-config container ir-a-juego)

; mostrar ventana
(send frame show #t)