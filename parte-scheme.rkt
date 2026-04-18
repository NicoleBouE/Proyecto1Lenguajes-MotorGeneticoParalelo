#lang racket

; --- GENERACIÓN DE INDIVIDUOS

; Dominio: número natural (0-100)
; Codominio: lista de 3 elementos con símbolo operador y dos listas vacías
(define elegir-operacion
  (lambda (rand)
    (cond ((< rand 25) '(+ () ()))
          ((< rand 50) '(- () ()))
          ((< rand 75) '(* () ()))
          (else '(/ () ())))))

; Dominio: número natural (0-200)
; Codominio: símbolo 'x, símbolo 'y, o número entero entre -10 y 10
(define elegir-letra-num
  (lambda (rand)
    (cond ((< rand 60) 'x)
          ((< rand 120) 'y)
          (else (- (random 21) 10)))))

; Dominio: Raiz (símbolo), elem (número o símbolo)
; Codominio: número o símbolo (evitando cero en división)
(define revisar-cero-division
  (lambda (Raiz elem)
    (cond ((and (number? elem) (and (zero? elem) (equal? Raiz '/)))
           (revisar-cero-division Raiz (elegir-letra-num (random 151))))
          (else elem))))

; Dominio: Lado (símbolo 'izq o 'der), Raiz (símbolo), operacion (lista)
; Codominio: lista de 3 elementos (operador y dos subárboles)
(define generar-subarbol-aux
  (lambda (Lado Raiz operacion)
    (list (car operacion)
          (generar-subarbol 'izq (car operacion) (random 101))
          (generar-subarbol 'der (car operacion) (random 101)))))

; Dominio: Lado (símbolo 'izq o 'der), Raiz (símbolo), rand (número natural 0-100)
; Codominio: subárbol (lista de 3 elementos) o atómico (número o símbolo)
(define generar-subarbol
  (lambda (Lado Raiz rand)
    (cond ((and (equal? Lado 'der) (< rand 55)) (revisar-cero-division Raiz (elegir-letra-num (random 151))))
          ((and (equal? Lado 'izq) (< rand 55)) (elegir-letra-num (random 151)))
          (else (generar-subarbol-aux Lado Raiz (elegir-operacion (random 101)))))))

; Dominio: Operacion (lista con operador en car y dos listas vacías)
; Codominio: individuo (lista de 3 elementos: operador, subárbol izquierdo, subárbol derecho)
(define generar-individuo
  (lambda (Operacion)
    (list (car Operacion)
          (generar-subarbol 'izq (car Operacion) (random 101))
          (generar-subarbol 'der (car Operacion) (random 101)))))

; Dominio: n (número natural)
; Codominio: lista de n individuos árboles sintácticos (población)
(define generar-poblacion
  (lambda (n)
    (cond ((zero? n) '())
          (else (cons (generar-individuo (elegir-operacion (random 101)))
                      (generar-poblacion (- n 1)))))))

; --- CRUCE

; Dominio: Padre1 (lista), Padre2 (lista), rand (número natural 0-100)
; Codominio: lista de 3 elementos con operador y dos listas vacías
(define escoger-raiz
  (lambda (Padre1 Padre2 rand) 
    (cond ((< rand 50) (list (car Padre1) '() '()))
          (else (list (car Padre2) '() '())))))

; Dominio: Padre (lista), rand (número natural 0-100)
; Codominio: subárbol (lista) o atómico (número o símbolo)
(define escoger-hijo
  (lambda (Padre rand)
    (cond ((< rand 20) (generar-individuo (elegir-operacion (random 101))))
          ((< rand 60) (cadr Padre))
          (else (caddr Padre)))))

; Dominio: Raiz (lista), Padre1 (lista), Padre2 (lista), rand (número natural 0-100)
; Codominio: lista de 3 elementos (operador y dos hijos cruzados)
(define cruzar-aux 
  (lambda (Raiz Padre1 Padre2 rand)
    (cond ((< rand 50) (list (car Raiz) (escoger-hijo Padre1 (random 101)) (escoger-hijo Padre2 (random 101))))
          (else (list (car Raiz) (escoger-hijo Padre2 (random 101)) (escoger-hijo Padre1 (random 101)))))))

; Dominio: Padre1 (lista), Padre2 (lista)
; Codominio: nuevo individuo (lista de 3 elementos) resultado del cruce
(define cruzar
  (lambda (Padre1 Padre2)
    (cruzar-aux (escoger-raiz Padre1 Padre2 (random 101)) Padre1 Padre2 (random 101))))

; --- TORNEO ---

; Dominio: ind1 (par (fitness . árbol)), ind2 (par (fitness . árbol))
; Codominio: el par con mayor fitness
(define obtener-mejor
  (lambda (ind1 ind2)
    (cond ((> (car ind1) (car ind2)) ind1)
          (else ind2))))

; Dominio: lst (lista no vacía)
; Codominio: elemento aleatorio de la lista
(define seleccionar-al-azar
  (lambda (lst)
    (list-ref lst (random (length lst)))))

; Dominio: poblacion (lista), k (número natural)
; Codominio: lista de k elementos seleccionados al azar (con repetición)
(define generar-competidores
  (lambda (poblacion k)
    (cond ((zero? k) '())
          (else (cons (seleccionar-al-azar poblacion)
                      (generar-competidores poblacion (- k 1)))))))

; Dominio: competidores (lista no vacía de pares (fitness . árbol))
; Codominio: el competidor con mayor fitness
(define encontrar-ganador
  (lambda (competidores)
    (foldl (lambda (actual acumulado) (obtener-mejor actual acumulado))
           (car competidores)
           (cdr competidores))))

; Dominio: poblacion-fitness (lista de pares (fitness . árbol)), k (número natural)
; Codominio: árbol del ganador del torneo, o #f si no hay ganador
(define seleccionar-padre-torneo
  (lambda (poblacion-fitness k)
    ((lambda (ganador)
       (cond ((pair? ganador) (cdr ganador))
             (else ganador)))
     (encontrar-ganador (generar-competidores poblacion-fitness k)))))

; --- CICLO EVOLUTIVO 

; Dominio: poblacion-fitness (lista de pares), n (natural), k (natural)
; Codominio: lista de n nuevos individuos generados por cruce
(define evolucionar-poblacion
  (lambda (poblacion-fitness n k)
    (cond ((<= n 0) '())
          (else (cons (cruzar (seleccionar-padre-torneo poblacion-fitness k)
                              (seleccionar-padre-torneo poblacion-fitness k))
                      (evolucionar-poblacion poblacion-fitness (- n 1) k))))))

; Dominio: resultados-evaluados (lista de pares), in (puerto), out (puerto), puntos (lista), generacion (natural), mejor-f (real), contador (natural), limite (natural)
; Codominio: void (efecto: ejecuta ciclo evolutivo)
(define ejecutar-ciclo-evolutivo
  (lambda (resultados-evaluados in out puntos generacion mejor-f contador limite)
    ((lambda (mejor-actual)
       (displayln (format "Generación ~a - Mejor Fitness: ~a" generacion (car mejor-actual)))
       (displayln (format "Mejor Árbol: ~a" (cdr mejor-actual)))
       (cond 
         ((or (>= (car mejor-actual) 0.99) (>= contador limite))
          (displayln "SISTEMA FINALIZADO.")
          (close-output-port out))
         (else
          ((lambda (nuevo-mejor-f nuevo-contador)
             (escribir-a-erlang 
              (construir-mensaje puntos 
                                 (cons (cdr mejor-actual) 
                                       (evolucionar-poblacion resultados-evaluados 
                                                              (- (length resultados-evaluados) 1) 15))) 
              out)
             (bucle-espera-evaluacion in out '() puntos (+ generacion 1) nuevo-mejor-f nuevo-contador limite))
           (cond ((> (car mejor-actual) mejor-f) (car mejor-actual)) (else mejor-f))
           (cond ((> (car mejor-actual) mejor-f) 0) (else (+ contador 1)))))))
     (encontrar-ganador resultados-evaluados))))

; --- FORMATO Y COMUNICACIÓN ---

; Dominio: mensaje (lista con estructura de resultados)
; Codominio: lista de pares (fitness . árbol)
(define procesar-resultados 
  (lambda (mensaje)
    (map (lambda (item) (cons (car item) (cdr item))) (cadr mensaje))))

; Dominio: Arbol (número, símbolo o lista)
; Codominio: string con representación del árbol
(define formatear-arbol 
  (lambda(Arbol)
    (cond ((number? Arbol) (number->string Arbol))
          ((symbol? Arbol) (symbol->string Arbol))
          ((list? Arbol) (string-append "(" (string-join (map formatear-arbol Arbol) " ") ")"))
          (else "0"))))

; Dominio: Puntos (lista de puntos (x y z)), Poblacion (lista de árboles)
; Codominio: string con formato para enviar a Erlang
(define construir-mensaje 
  (lambda (Puntos Poblacion)
    (string-append "{puntos, [" 
                   (string-join (map (lambda (p) (format "{~a,~a,~a}" (car p) (cadr p) (caddr p))) Puntos) ",")
                   "], arboles, ["
                   (string-join (map (lambda (a) (string-append "\"" (formatear-arbol a) "\"")) Poblacion) ",")
                   "]}.")))

; Dominio: mensaje (string), out (output-port)
; Codominio: void (efecto: escribe mensaje con prefijo de longitud)
(define escribir-a-erlang
  (lambda (mensaje out)
    ((lambda (b)
       (write-bytes (integer->integer-bytes (bytes-length b) 4 #f #t) out)
       (write-bytes b out)
       (flush-output out))
     (string->bytes/utf-8 mensaje))))

; Dominio: in (input-port)
; Codominio: S-expression leído, o #f si EOF
(define leer-mensaje-erlang
  (lambda (in)
    ((lambda (len-bytes)
       (cond
         ((eof-object? len-bytes) #f)
         (else
          ((lambda (len)
             (read (open-input-string (bytes->string/utf-8 (read-bytes len in)))))
           (integer-bytes->integer len-bytes #f #t)))))
     (read-bytes 4 in))))

; Dominio: mensaje (S-expression), in (puerto), out (puerto), poblacion (lista),
;         puntos (lista), generacion (natural), mejor-f (real), contador (natural), limite (natural)
; Codominio: void (efecto: procesa mensaje y continúa bucle)
(define procesar-mensaje
  (lambda (mensaje in out poblacion puntos generacion mejor-f contador limite)
    (cond
      ((and (list? mensaje) (equal? (car mensaje) 'compute))
       (escribir-a-erlang (format "{result, ~a}." 
                                  (* 1.0 (with-handlers ([exn:fail? (lambda (e) 0.0)])
                                           (eval `((lambda (x y) ,(caddr mensaje)) ,(caadr mensaje) ,(cadadr mensaje)) entorno))))
                          out)
       (bucle-espera-evaluacion in out poblacion puntos generacion mejor-f contador limite))
      ((and (list? mensaje) (equal? (car mensaje) 'results))
       (ejecutar-ciclo-evolutivo (procesar-resultados mensaje) in out puntos generacion mejor-f contador limite))
      (else (bucle-espera-evaluacion in out poblacion puntos generacion mejor-f contador limite)))))

; Dominio: in (puerto), out (puerto), poblacion (lista), puntos (lista),
;         generacion (natural), mejor-f (real), contador (natural), limite (natural)
; Codominio: void (efecto: espera mensaje de Erlang y lo procesa)
(define bucle-espera-evaluacion
  (lambda (in out poblacion puntos generacion mejor-f contador limite)
    ((lambda (msg)
       (cond
         (msg (procesar-mensaje msg in out poblacion puntos generacion mejor-f contador limite))
         (else (void))))
     (leer-mensaje-erlang in))))

; Dominio: entorno para evaluación de expresiones
; Codominio: namespace de Racket
(define entorno (make-base-namespace))

; Dominio: host (string), puerto (número), puntos (lista de puntos (x y z)), tamano-pob (natural)
; Codominio: void (efecto: inicia sistema evolutivo conectando con Erlang)
(define iniciar-sistema-evolutivo
  (lambda (host puerto puntos tamano-pob)
    ((lambda (pob)
       (call-with-values (lambda () (tcp-connect host puerto))
                         (lambda (in out)
                           (escribir-a-erlang (construir-mensaje puntos pob) out)
                           (bucle-espera-evaluacion in out pob puntos 0 0 0 10))))
     (generar-poblacion tamano-pob))))

; Dominio: una lista de listas que contiene tres números
; Codominio: void
(define resolver-puntos
  (lambda (puntos)
    ((lambda (host puerto)
       ((lambda (n-puntos)
          ((lambda (tamano-dinamico)
             (displayln "---------------------------------------")
             (displayln (format "Puntos recibidos: ~a" n-puntos))
             (displayln (format "Población asignada: ~a" tamano-dinamico))
             (displayln "---------------------------------------")
             (iniciar-sistema-evolutivo host puerto puntos tamano-dinamico))
           (max 50 (inexact->exact (ceiling (sqrt n-puntos))))))
        (length puntos)))
     "localhost" 8000)))
