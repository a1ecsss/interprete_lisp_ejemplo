
(defun hanoi (n a b c)
  (format t "Disco # ~D: " n) 
  (if (= n 1)
      (format t "Mover de ~A a ~A ~%" a c )  ;; Imprime el movimiento cuando queda un solo disco
      ((hanoi (- n 1) a c b)  ;; Mueve n-1 discos de A a B usando C como auxiliar
        (hanoi 1 a b c)        ;; Mueve el disco más grande de A a C
        (hanoi (- n 1) b a c)))) ;; Mueve los n-1 discos de B a C usando A como auxiliar

;; Llamada de prueba con 3 discos
(write "Ingrese la cantidad de discos: ")
(hanoi (read) "Torre 1" "Torre 2" "Torre 3")
;;(hanoi 4 "Torre 1" "Torre 2" "Torre 3" "")

;(print '( a b c))
;(print (list 1 2 3))       ; → (1 2 3)
;(print (car '(a b c)))     ; → a
;(print (cdr '(a b c)))     ; → (b c)
;(print (cons 'x '(y z)))   ; → (x y z)
;(print (append '(1 2) '(3 4)))  ; → (1 2 3 4)
;(print (length '(5 6 7 8)))  ; → 4
;(print (reverse '(1 2 3)))  ; → (3 2 1)
;(print (expt 2 2))
;(print (atom '(1 2)))
;(setq x 3)
;(setq x 4)
;(cond 
;  ((= x 1) (print "Es uno"))   ; ✅ Correcto
;  ((= x 2) (print "Es dos"))   ; ✅ Correcto
;  (t (print "otro valor")))    ; ✅ Correcto

;(setq edad 18)
;(setq edad 16)
;(case edad
;  (16 (print "Puedes obtener tu licencia"))
;  (18 (print "Eres mayor de edad"))
;  (21 (print "Puedes beber legalmente en EE.UU."))
;  (otherwise (print "Nada especial en esta edad")))

;(setq lista '(a b c))
;(print lista)
;(setf (car lista) 'z) 
;(setf (cdr lista) '(y z))
;(print lista)


;(print "Hola Mundo")  ; → "Hola Mundo"
;(print '(1 2 3))      ; → (1 2 3)
;(write "Hola") 
;(write " Mundo")

;(format T "Hola, ~A" 3)  ; → "Hola, Juan"
;(format NIL "Pi: ~F" 3.1415)  ; → Devuelve "Pi: 3.14" pero no lo imprime
;(setq x (read))  ; Espera entrada del usuario
;(print x)  ; Muestra lo ingresado

(format t "Hola, ~A" "Mundo")
(format t "Precio: ~8,3F dólares" 12.3456)
