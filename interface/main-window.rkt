#lang racket/gui

; importar pantallas
(require "config-screen.rkt")
(require "game-screen.rkt")

; ventana principal
(define frame (new frame%
                   [label "BusCEMinas"]
                   [width 1150]
                   [height 850]))

; contenedor para las pantallas
(define container (new panel%
                      [parent frame]))

; funcion auxiliar recursiva para procesar la lista
(define (ocultar-hijos lista-hijos)
  (cond
    [(null? lista-hijos) #t]
    [else
     (send (car lista-hijos) show #f) 
     (ocultar-hijos (cdr lista-hijos))]))

; funcion para limpiar contenedor
(define (limpiar-contenedor)
  (ocultar-hijos (send container get-children)))

; funcion para ir al juego
(define (ir-a-juego filas columnas dificultad)
  (limpiar-contenedor)
  (crear-pantalla-juego container volver-a-config filas columnas dificultad))

; funcion para volver a configuración
(define (volver-a-config)
  (limpiar-contenedor)
  (crear-pantalla-config container ir-a-juego))

; iniciar con pantalla de configuracion
(crear-pantalla-config container ir-a-juego)

; mostrar ventana
(send frame show #t)