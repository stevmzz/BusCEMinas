#lang racket/gui

; importar la logica del tablero desde el modulo generador
(require "../logic/board-generator.rkt")

; funcion principal exportada para crear la interfaz del juego
(provide crear-pantalla-juego)

(define (crear-pantalla-juego parent-container callback-volver filas columnas dificultad)
  ; convertir strings de entrada a valores numericos
  (define tamaño-filas (string->number filas))
  (define tamaño-columnas (string->number columnas))
  
  ; crear el tablero usando la logica del generador
  (define mi-tablero (tablero tamaño-filas tamaño-columnas dificultad))
  ; calcular el numero total de minas colocadas en el tablero
  (define total-minas (contar-minas-tablero mi-tablero))
  
  ; variable de estado para controlar el modo de juego actual
  (define modo-bandera? #f)  ; #f = modo descubrir, #t = modo bandera
  
  ; contenedor principal que organiza toda la interfaz verticamente
  (define game-panel (new vertical-panel%
                         [parent parent-container]
                         [spacing 10]))
  
  ; === seccion superiror con titulo y controles ===
  
  ; panel que contiene el titulo y los controles del juego
  (define header-panel (new vertical-panel%
                           [parent game-panel]
                           [spacing 10]
                           [border 15]
                           [stretchable-height #f]))
  
  ; lienzo para dibujar el titulo del juego con estilo personalizado
  (define title-canvas (new canvas%
                           [parent header-panel]
                           [min-height 80]
                           [stretchable-height #f]
                           [paint-callback
                            (lambda (canvas dc)
                              (send dc set-brush "black" 'solid)
                              (send dc draw-rectangle 0 0 1103 80)
                              (send dc set-scale 2 2)
                              (send dc set-text-foreground "white")
                              (send dc draw-text "BusCEMinas" 235 10))]))
  
  ; contenedor horizontal para organizar controles en linea
  (define info-panel (new horizontal-panel%
                         [parent header-panel]
                         [alignment '(center center)]
                         [spacing 20]
                         [stretchable-height #f]))
  
  ; etiqueta que muestra el contador de minas restantes
  (define contador-minas-label (new message%
                                   [parent info-panel]
                                   [label "Minas: 0"]
                                   [min-width 120]))
  
  ; boton que permite alternar entre modo descubrir y modo bandera
  (define boton-modo (new button%
                         [parent info-panel]
                         [label "Modo: Descubrir"]
                         [min-width 120]
                         [callback (lambda (b e)
                                   ; cambiar el estado del modo de juego
                                   (set! modo-bandera? (not modo-bandera?))
                                   ; actualizar texto del boton segun el modo activo
                                   (send boton-modo set-label 
                                         (if modo-bandera? "Modo: Bandera" "Modo: Descubrir")))]))
  
  ; boton para regresar a la pantalla de configuracion inicial
  (define boton-menu (new button%
                         [parent info-panel]
                         [label "Volver al Menú"]
                         [min-width 120]
                         ; ejecutar funcion callback para navegar de vuelta
                         [callback (lambda (b e)
                                   (callback-volver))]))
  
  ; === seccion del tablero de juego ===
  
  ; contenedor principal del tablero que organiza columnas horizontalmente
  (define tablero-panel (new horizontal-panel%
                            [parent game-panel]
                            [alignment '(center center)]
                            [spacing 2]
                            [border 10]))
  
  ; funcion que maneja los clicks en las celdas segun el modo activo
  (define (procesar-click-celda boton es-mina num-adyacentes indice)
    ; obtener el estado actual de la celda (etiqueta del boton)
    (define label-actual (send boton get-label))
    
    ; procesar click segun el modo de juego seleccionado
    (if modo-bandera?
        ; logica para modo bandera: colocar/quitar banderas
        (cond
          [(equal? label-actual "⚑") ; si ya tiene bandera, quitarla
           (send boton set-label "")
           (actualizar-contador-minas)]
          [(equal? label-actual "") ; si esta vacia, colocar bandera
           (send boton set-label "⚑")
           (actualizar-contador-minas)]
          [else (void)]) ; si ya esta descubierta, no hacer nada
        
        ; logica para modo descubrir: revelar contenido de celdas
        (cond
          [(equal? label-actual "⚑") ; no permitir descubrir celdas con bandera
           (void)]
          [(equal? label-actual "") ; celda no descubierta, proceder a revelar
           (if (= es-mina 1)
               ; si es mina, mostrar "MINA" y terminar juego
               (begin
                 (send boton set-label "💣"))
               ; si no es mina, mostrar numero de minas adyacentes
               (begin
                 (send boton set-label (number->string num-adyacentes))
                 ; si no hay minas adyacentes, descubrir vecinos automaticamente
                 (when (= num-adyacentes 0)
                   (auto-descubrir-vecinos indice))))]
          [else (void)]))) ; celda ya descubierta, ignorar click
  
  ; funcion recursiva para crear todos los botones de una columna
  (define (crear-botones-columna col-panel columna-index)
    ; funcion auxiliar que crea botones fila por fila
    (define (crear-boton-fila fila-index)
      (cond
        ; caso base: ya se crearon todas las filas
        [(>= fila-index tamaño-filas) '()]
        [else
         ; calcular posicion lineal de la celda en el tablero
         (define indice (+ (* fila-index tamaño-columnas) columna-index))
         ; obtener informacion de la celda desde la logica del juego
         (define celda-datos (elemento mi-tablero indice))
         ; extraer propiedades importantes de la celda
         (define es-mina (mine celda-datos))
         (define num-adyacentes (second celda-datos))
         
         ; crear el boton de la celda con sus propiedades
         (cons (new button%
                    [parent col-panel]
                    [label ""] ; inicialmente vacio (no descubierto)
                    [min-height 35]
                    [min-width 35]
                    [horiz-margin 1]
                    [vert-margin 1]
                    ; asignar manejador de clicks con datos de la celda
                    [callback (lambda (b event)
                              (procesar-click-celda b es-mina num-adyacentes indice))])
               ; llamada recursiva para crear el siguiente boton de la fila
               (crear-boton-fila (+ fila-index 1)))]))
    
    ; iniciar la creacion desde la primera fila
    (crear-boton-fila 0))
  
  ; funcion recursiva para crear todas las columnas del tablero
  (define (crear-columnas-tablero columna-index)
    (cond
      ; caso base: ya se crearon todas las columnas
      [(>= columna-index tamaño-columnas) '()]
      [else
       ; crear contenedor vertical para los botones de esta columna
       (define col-panel (new vertical-panel%
                             [parent tablero-panel]
                             [spacing 1]
                             [stretchable-width #f]))
       
       ; generar todos los botones para la columna actual
       (define botones-columna (crear-botones-columna col-panel columna-index))
       ; agregar esta columna y continuar con la siguiente recursivamente
       (cons botones-columna 
             (crear-columnas-tablero (+ columna-index 1)))]))
  
  ; inicializar y construir toda la matriz de botones del tablero
  (define celdas (crear-columnas-tablero 0))
  
  ; === funciones auxiliares para la logica del juego ===

  ; funcion para localizar un boton especifico usando su indice lineal
  (define (obtener-boton indice)
    ; convertir indice lineal a coordenadas de fila y columna
    (define fila (quotient indice tamaño-columnas))
    (define col (remainder indice tamaño-columnas))
    ; navegar por la estructura de datos para encontrar el boton
    (define lista-columna (elemento celdas col))
    (elemento lista-columna fila))
  
  ; funcion recursiva para descubrir automaticamente celdas vecinas vacias
  (define (auto-descubrir-vecinos indice-inicial)
    ; convertir indice a coordenadas para calcular vecinos
    (define fila (quotient indice-inicial tamaño-columnas))
    (define col (remainder indice-inicial tamaño-columnas))
    ; obtener lista de coordenadas de todas las celdas vecinas validas
    (define vecinos-coords (vecinos fila col tamaño-filas tamaño-columnas))
    
    ; funcion para procesar una celda vecina individual
    (define (procesar-vecino vecino-coord)
      ; extraer coordenadas del vecino
      (define v-fila (first vecino-coord))
      (define v-col (second vecino-coord))
      ; calcular indice lineal del vecino
      (define v-indice (+ (* v-fila tamaño-columnas) v-col))
      ; obtener referencia al boton del vecino
      (define v-boton (obtener-boton v-indice))
      ; verificar estado actual del boton vecino
      (define v-label (send v-boton get-label))
      
      ; procesar solo si la celda vecina no ha sido descubierta
      (when (equal? v-label "")
        ; obtener datos de la celda vecina desde la logica
        (define v-celda (elemento mi-tablero v-indice))
        (define v-es-mina (mine v-celda))
        (define v-adyacentes (second v-celda))
        
        ; descubrir solo si no es mina
        (when (= v-es-mina 0)
          ; revelar el numero de minas adyacentes
          (send v-boton set-label (number->string v-adyacentes))
          ; si tampoco tiene minas adyacentes, continuar expansion recursiva
          (when (= v-adyacentes 0)
            (auto-descubrir-vecinos v-indice)))))
    
    ; funcion auxiliar para iterar sobre todos los vecinos
    (define (procesar-lista-vecinos lista-vecinos)
      (cond
        [(null? lista-vecinos) (void)] ; caso base: no mas vecinos
        [else
         ; procesar el primer vecino de la lista
         (procesar-vecino (car lista-vecinos))
         ; continuar con el resto de vecinos recursivamente
         (procesar-lista-vecinos (cdr lista-vecinos))]))
    
    ; iniciar el procesamiento de todos los vecinos
    (procesar-lista-vecinos vecinos-coords))
  
  ; funcion recursiva para contar el numero actual de banderas colocadas
  (define (contar-banderas-actuales)
    ; funcion auxiliar para contar banderas en una columna de botones
    (define (contar-banderas-columna lista-botones)
      (cond
        [(null? lista-botones) 0] ; caso base: no mas botones en la columna
        [else
         ; obtener etiqueta del primer boton de la lista
         (define boton (car lista-botones))
         (define label (send boton get-label))
         ; sumar 1 si es bandera, 0 si no, y continuar con el resto
         (+ (if (equal? label "⚑") 1 0)
            (contar-banderas-columna (cdr lista-botones)))]))
    
    ; funcion auxiliar para iterar sobre todas las columnas
    (define (contar-todas-columnas lista-columnas)
      (cond
        [(null? lista-columnas) 0] ; caso base: no mas columnas
        [else
         ; sumar banderas de la primera columna y continuar recursivamente
         (+ (contar-banderas-columna (car lista-columnas))
            (contar-todas-columnas (cdr lista-columnas)))]))
    
    ; iniciar conteo desde todas las columnas del tablero
    (contar-todas-columnas celdas))
  
  ; funcion para actualizar la visualizacion del contador de minas
  (define (actualizar-contador-minas)
    ; calcular minas restantes basado en banderas colocadas
    (define banderas-actuales (contar-banderas-actuales))
    (define minas-restantes (- total-minas banderas-actuales))
    ; actualizar texto del contador en la interfaz
    (send contador-minas-label set-label (format "Minas: ~a" minas-restantes)))
  
  ; configurar estado inicial del contador al crear la pantalla
  (actualizar-contador-minas)
  
  ; devolver el panel principal como resultado de la funcion
  game-panel)