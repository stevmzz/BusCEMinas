#lang racket

; exportar todas las funciones definidas para uso en otros modulos
(provide (all-defined-out))

; funcion para crear un tablero vacio con celdas inicializadas
(define (crear-tablero filas columnas)
  ; calcular total de celdas necesarias en el tablero
  (define total (* filas columnas))
  
  ; funcion auxiliar recursiva para construir lista de celdas vacias
  (define (construir-lista n)
    (cond
      ; caso base: no mas celdas por crear
      [(= n 0) '()]
      ; caso recursivo: crear celda vacia y continuar
      [else (cons (list 0 0) (construir-lista (sub1 n)))]))

  ; iniciar construccion del tablero con el total de celdas
  (construir-lista total))

; funcion para verificar si una celda contiene una mina
; estructura de celda: (es-mina? minas-adyacentes)
(define (mine celda)
  ; retornar primer elemento que indica presencia de mina
  (first celda))

; funcion recursiva para obtener elemento en posicion especifica de lista
(define (elemento lista index)
  (cond
    ; error si se intenta acceder fuera de los limites
    [(null? lista) (error "Índice fuera de límites")]
    ; caso base: encontrar elemento en posicion 0
    [(= index 0) (car lista)]
    ; caso recursivo: avanzar en la lista decrementando indice
    [else (elemento (cdr lista) (sub1 index))]))

; funcion recursiva para reemplazar elemento en posicion especifica
(define (reemplazar-en-lista lst index new-element)
  (cond
    ; lista vacia, retornar lista vacia
    [(null? lst) '()]
    ; posicion encontrada, reemplazar elemento
    [(= index 0) (cons new-element (cdr lst))]
    ; caso recursivo: mantener elemento actual y continuar
    [else (cons (car lst) (reemplazar-en-lista (cdr lst) (sub1 index) new-element))]))

; funcion para distribuir minas aleatoriamente en el tablero
(define (colocar-minas tablero filas columnas num-minas)
  ; calcular total de posiciones disponibles
  (define total (* filas columnas))

  ; funcion auxiliar recursiva para colocar n minas
  (define (colocar-n-minas n current-tablero)
    (cond
      ; caso base: todas las minas colocadas
      [(= n 0) current-tablero] 
      [else 
        ; generar posicion aleatoria en el tablero
        (define pos (random total))
        ; obtener celda en la posicion generada
        (define celda (elemento current-tablero pos)) 

        (cond
          ; si ya hay mina, intentar otra posicion
          [(= (mine celda) 1) 
           (colocar-n-minas n current-tablero)]
          ; si posicion libre, colocar mina y continuar
          [else 
           (colocar-n-minas (sub1 n) (reemplazar-en-lista current-tablero pos (list 1 0)))]
        )]))
  
  ; iniciar colocacion con el numero especificado de minas
  (colocar-n-minas num-minas tablero))

; funcion para obtener coordenadas de celdas vecinas validas
(define (vecinos i j filas columnas)
  ; definir todos los desplazamientos posibles (8 direcciones)
  (define desplazamientos '((-1 -1) (-1 0) (-1 1)
                             (0 -1)          (0 1)
                             (1 -1)  (1 0)  (1 1)))
                             
  ; funcion auxiliar para procesar cada desplazamiento
  (define (recorrer-desplazamientos lista-d)
    (cond
      ; caso base: no mas desplazamientos por procesar
      [(null? lista-d) '()]
      [else
       ; obtener primer desplazamiento de la lista
       (define d (car lista-d))
       ; calcular nuevas coordenadas aplicando desplazamiento
       (define ni (+ i (first d)))
       (define nj (+ j (second d)))
       
       (cond
         ; verificar que las coordenadas esten dentro de los limites
         [(and (<= 0 ni (sub1 filas))
               (<= 0 nj (sub1 columnas)))
          ; coordenadas validas, agregar a la lista y continuar
          (cons (list ni nj) (recorrer-desplazamientos (cdr lista-d)))]
         ; coordenadas fuera de limites, continuar sin agregar
         [else
          (recorrer-desplazamientos (cdr lista-d))]
       )]))
       
  ; iniciar procesamiento de todos los desplazamientos
  (recorrer-desplazamientos desplazamientos))

; funcion auxiliar recursiva para contar minas en celdas vecinas
(define (contar-minas-vecinas tablero lista-vecinos columnas)
  (cond
    ; caso base: no mas vecinos por revisar
    [(null? lista-vecinos) 0]
    [else
      ; obtener coordenadas del primer vecino
      (define v (car lista-vecinos))
      (define vi (first v))
      (define vj (second v))
      ; convertir coordenadas a indice lineal
      (define pos (+ (* vi columnas) vj))
      
      ; obtener datos de la celda vecina
      (define celda-vecina (elemento tablero pos)) 
      
      ; sumar 1 si es mina, 0 si no, y continuar recursivamente
      (+ (cond [(= (mine celda-vecina) 1) 1] [else 0])
         (contar-minas-vecinas tablero (cdr lista-vecinos) columnas))]
    ))

; funcion para calcular numero de minas adyacentes para cada celda
(define (calcular-adyacentes tablero filas columnas)
  ; calcular total de celdas en el tablero
  (define total (* filas columnas))
  
  ; funcion auxiliar recursiva para iterar sobre todas las posiciones
  (define (iterar-tablero idx current-tablero)
    (cond
      ; caso base: todas las posiciones procesadas
      [(= idx total) current-tablero] 
      [else
        ; convertir indice lineal a coordenadas de fila y columna
        (define i (quotient idx columnas))
        (define j (remainder idx columnas))
        ; obtener datos de la celda actual
        (define celda (elemento current-tablero idx))
        
        (cond
          ; si es mina, no calcular adyacentes, continuar
          [(= (mine celda) 1) 
           (iterar-tablero (add1 idx) current-tablero)]
          ; si no es mina, calcular minas adyacentes
          [else 
           ; obtener coordenadas de todos los vecinos validos
           (define vecinos-coords (vecinos i j filas columnas))
           ; contar cuantas minas hay en las celdas vecinas
           (define count (contar-minas-vecinas current-tablero vecinos-coords columnas))
           ; crear nueva celda con el conteo actualizado
           (define nueva-celda (list 0 count))
           
           ; actualizar tablero con la nueva informacion
           (define nuevo-tablero-actualizado 
             (reemplazar-en-lista current-tablero idx nueva-celda))
             
           ; continuar con la siguiente posicion
           (iterar-tablero (add1 idx) nuevo-tablero-actualizado)]
        )]))
        
  ; iniciar iteracion desde la primera posicion
  (iterar-tablero 0 tablero)) 


; funcion principal para generar tablero completo con minas y conteos
(define (tablero filas columnas nivel)
  ; calcular numero total de celdas
  (define total (* filas columnas))
  ; determinar porcentaje de minas segun nivel de dificultad
  (define porcentaje
    (cond
      [(equal? nivel "facil") 0.10]   ; 10% de minas
      [(equal? nivel "medio") 0.15]   ; 15% de minas
      [(equal? nivel "dificil") 0.20] ; 20% de minas
      [else (error "Nivel no reconocido: use 'facil, 'medio o 'dificil")]))
  ; calcular numero exacto de minas (minimo 1)
  (define num-minas (max 1 (round (* total porcentaje))))
  
  ; pipeline funcional para construir tablero completo
  ; paso 1: crear tablero vacio
  (define tablero-vacio (crear-tablero filas columnas))
  ; paso 2: colocar minas aleatoriamente
  (define tablero-con-minas (colocar-minas tablero-vacio filas columnas num-minas))
  ; paso 3: calcular numeros adyacentes para cada celda
  (calcular-adyacentes tablero-con-minas filas columnas))


  
; funcion recursiva para contar total de minas en tablero generado
(define (contar-minas-tablero tablero)
  (cond
    ; caso base: lista vacia, no mas celdas por revisar
    [(null? tablero) 0]
    [else
     ; obtener primera celda de la lista
     (define celda (car tablero))
     ; verificar si contiene mina
     (define es-mina (mine celda))
     ; sumar mina actual y continuar recursivamente con resto
     (+ es-mina (contar-minas-tablero (cdr tablero)))]))