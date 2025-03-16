;;"C:\Program Files\Java\jdk-22\bin\java" Main
(defun hanoi (n a b c)
  (if (= n 1)
      (format t "Mover de ~A a ~A ~%" a c )  ;; Imprime el movimiento cuando queda un solo disco
      ((hanoi (- n 1) a c b)  ;; Mueve n-1 discos de A a B usando C como auxiliar
        (hanoi 1 a b c)        ;; Mueve el disco más grande de A a C
        (hanoi (- n 1) b a c)))) ;; Mueve los n-1 discos de B a C usando A como auxiliar

;; Llamada de prueba con 3 discos
(write "Ingrese la cantidad de discos: ")
(hanoi (read) "Torre 1" "Torre 2" "Torre 3")

;; Definir una función recursiva para calcular la suma de los primeros n números
(defun suma-recursiva (n)
  (if (<= n 0)
      0
      (+ n (suma-recursiva (- n 1)))))

;; Leer un número del usuario
(format t "Ingrese un número entero positivo: ")
(setq numero (read))
;; Verificar si el número es positivo
(cond 
  ((not (numberp numero)) (format t "Error: Ingrese un número válido.~%"))
  ((< numero 1) (format t "Error: Ingrese un número positivo.~%"))
  (t 
   ;; Operaciones matemáticas
   (setq suma (suma-recursiva numero))
   (setq doble (* 2 numero))
   (setq mitad (/ numero 2))

   ;; Manipulación de listas
   (setq lista (list 1 2 3 4 5))
   (setq lista-invertida (reverse lista))
   (setq lista-expandida (append lista '(6 7 8)))
   ;; Comparaciones
   (setq es-mayor (if (> numero 10) "Sí" "No"))
   ;; Variables y lógica
   (setq es-par (if (= (mod numero 2) 0) t nil))

   ;; Imprimir los resultados
   (format t "La suma de los primeros ~D números es ~D.~%" numero suma)
   (format t "El doble de ~D es ~D y la mitad es ~F.~%" numero doble mitad)
   (format t "Lista original: ~S ~%" lista)
   (format t "Lista invertida: ~S ~%" lista-invertida)
   (format t "Lista expandida: ~S ~%" lista-expandida)
   (format t "¿El número es mayor a 10? ~A~%" es-mayor)
   (format t "¿El número es par? ~A~%" es-par))
)

(setq edad 19)

(case edad
  (16 
    (print "Tienes 16 años.") 
    (print "Aún no puedes votar."))

  ((18 19) 
    (print "Eres mayor de edad.") 
    (print "Ahora puedes votar.") 
    (print "También puedes obtener tu licencia de conducir."))

  (21 
    (print "Puedes beber legalmente en EE.UU.")
    (print "También eres oficialmente un adulto en muchos países."))

  (otherwise 
    (print "Edad no relevante.")))

