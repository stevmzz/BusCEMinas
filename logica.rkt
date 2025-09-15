#lang racket/gui
; Solo pruebas para generar listas, no implementado con parte grafica
(define (randomlist length mx)
  (for/list ((i length))
    (random mx)))

(define (mine n)
  (first n))

#|lista random con dos elementos cada uno; primer elemento: 0 y 1 si son minas
segundo elemento: numero aleatoria de minas adyacentes|#
(define (tablero row columns number_mines)
  (for*/list ([i row]
              [j columns])
    (list (random 2) (random number_mines))))
