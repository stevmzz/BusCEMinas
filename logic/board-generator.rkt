#lang racket

(provide (all-defined-out))


; Crear tablero vacío (Lista de celdas)
(define (crear-tablero filas columnas)
  (define total (* filas columnas))
  
  ; Función  para construir la lista de tamaño N
  (define (construir-lista n)
    (cond
      [(= n 0) '()]
      [else (cons (list 0 0) (construir-lista (sub1 n)))]))

  (construir-lista total))

; Devuelve si una celda (list 1 0) o (list 0 N) tiene una mina (1).
(define (mine celda)
  (first celda))

; Obtener el elemento en el índice 
(define (elemento lista index)
  (cond
    [(null? lista) (error "Índice fuera de límites")]
    [(= index 0) (car lista)]
    [else (elemento (cdr lista) (sub1 index))]))

; Reemplazar un elemento en una lista por índice
(define (reemplazar-en-lista lst index new-element)
  (cond
    [(null? lst) '()]
    [(= index 0) (cons new-element (cdr lst))]
    [else (cons (car lst) (reemplazar-en-lista (cdr lst) (sub1 index) new-element))]))

; Colocar minas aleatorias 
(define (colocar-minas tablero filas columnas num-minas)
  (define total (* filas columnas))

  ; Función para colocar N minas
  (define (colocar-n-minas n current-tablero)
    (cond
      [(= n 0) current-tablero] 
      [else 
        (define pos (random total))
        (define celda (elemento current-tablero pos)) 

        (cond
          [(= (mine celda) 1) 
           (colocar-n-minas n current-tablero)]
          [else 
           (colocar-n-minas (sub1 n) (reemplazar-en-lista current-tablero pos (list 1 0)))]
        )]))
  
  (colocar-n-minas num-minas tablero))


; Vecinos válidos de una celda 
(define (vecinos i j filas columnas)
  (define desplazamientos '((-1 -1) (-1 0) (-1 1)
                             (0 -1)          (0 1)
                             (1 -1)  (1 0)  (1 1)))
                             
  (define (recorrer-desplazamientos lista-d)
    (cond
      [(null? lista-d) '()]
      [else
       (define d (car lista-d))
       (define ni (+ i (first d)))
       (define nj (+ j (second d)))
       
       (cond
         [(and (<= 0 ni (sub1 filas))
               (<= 0 nj (sub1 columnas)))
          (cons (list ni nj) (recorrer-desplazamientos (cdr lista-d)))]
         [else
          (recorrer-desplazamientos (cdr lista-d))]
       )]))
       
  (recorrer-desplazamientos desplazamientos))

; Auxiliar: Cuenta las minas vecinas 
(define (contar-minas-vecinas tablero lista-vecinos columnas)
  (cond
    [(null? lista-vecinos) 0]
    [else
      (define v (car lista-vecinos))
      (define vi (first v))
      (define vj (second v))
      (define pos (+ (* vi columnas) vj))
      
      (define celda-vecina (elemento tablero pos)) 
      
      (+ (cond [(= (mine celda-vecina) 1) 1] [else 0])
         (contar-minas-vecinas tablero (cdr lista-vecinos) columnas))]
    ))

; Calcular adyacentes 
(define (calcular-adyacentes tablero filas columnas)
  (define total (* filas columnas))
  
  ; Función para iterar sobre todos los índices del tablero
  (define (iterar-tablero idx current-tablero)
    (cond
      [(= idx total) current-tablero] 
      [else
        (define i (quotient idx columnas))
        (define j (remainder idx columnas))
        (define celda (elemento current-tablero idx))
        
        (cond
          [(= (mine celda) 1) 
           (iterar-tablero (add1 idx) current-tablero)]
          [else 
           (define vecinos-coords (vecinos i j filas columnas))
           (define count (contar-minas-vecinas current-tablero vecinos-coords columnas))
           (define nueva-celda (list 0 count))
           
           (define nuevo-tablero-actualizado 
             (reemplazar-en-lista current-tablero idx nueva-celda))
             
           (iterar-tablero (add1 idx) nuevo-tablero-actualizado)]
        )]))
        
  (iterar-tablero 0 tablero)) 

; Función tablero principal 
(define (tablero filas columnas nivel)
  (define total (* filas columnas))
  (define porcentaje
    (cond
      [(equal? nivel "facil") 0.10]
      [(equal? nivel "medio") 0.15]
      [(equal? nivel "dificil") 0.20]
      [else (error "Nivel no reconocido: use 'facil, 'medio o 'dificil")]))
  (define num-minas (max 1 (round (* total porcentaje))))
  
  ; El pipeline funcional
  (define tablero-vacio (crear-tablero filas columnas))
  (define tablero-con-minas (colocar-minas tablero-vacio filas columnas num-minas))
  (calcular-adyacentes tablero-con-minas filas columnas))
  
