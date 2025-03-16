(defparameter *max-disk-size* 10) ;; Tamaño del disco más grande
(defparameter *max-disks* 5) ;; Límite máximo de discos
(defvar *towers* (make-hash-table))

(defun initialize-towers (n)
  "Inicializa las torres con los discos."
  (clrhash *towers*)
  (setf (gethash 'A *towers*) (loop for i from 0 below n
                                    collect (make-string (- *max-disk-size* (* i 2)) :initial-element #\#)))
  (setf (gethash 'B *towers*) '())
  (setf (gethash 'C *towers*) '())
  (print-towers n))

(defun can-move-disk? (from)
  "Verifica si hay un disco en la torre 'from' para mover."
  (and (gethash from *towers*) (not (null (gethash from *towers*)))))

(defun get-smallest-disk (tower)
  "Obtiene el tamaño del disco más pequeño en la torre dada."
  (if (can-move-disk? tower)
      (length (car (gethash tower *towers*)))
      nil))

(defun move-disk (from to)
  "Mueve el disco más pequeño disponible de una torre a otra si es válido."
  (when (can-move-disk? from)
    (let ((disk (pop (gethash from *towers*)))
          (smallest-target (get-smallest-disk to)))
      ;; Solo mover si la torre destino está vacía o si el disco es más pequeño
      (when (or (null smallest-target) (< (length disk) smallest-target))
        (push disk (gethash to *towers*))
        (sleep 1) ;; Pausa para animación
        (print-towers *max-disks*))))))

(defun print-towers (n)
  "Imprime la representación visual de las torres en la consola."
  (clear-screen)
  (let ((height (max n (length (gethash 'B *towers*)) (length (gethash 'C *towers*)))))
    (loop for i from (1- height) downto 0 do
      (format t " ~A~15T ~A~30T ~A~%"
              (format-disk (nth i (gethash 'A *towers*)))
              (format-disk (nth i (gethash 'B *towers*)))
              (format-disk (nth i (gethash 'C *towers*))))))

  (format t "------------~15T------------~30T------------~%") ;; Base de 12 caracteres
  (format t "     A~15T     B~30T     C~%")) ;; Nombres de torres

(defun format-disk (disk)
  "Centra el disco visualmente."
  (if disk
      (let* ((size (length disk))
             (padding (/ (- *max-disk-size* size) 2)))
        (concatenate 'string (make-string padding :initial-element #\Space) disk (make-string padding :initial-element #\Space)))
      "    ")) ;; Espacio vacío centrado

(defun clear-screen ()
  "Limpia la pantalla para animación sin errores en SBCL."
  #+win32 (progn (format t "~c[2J~c[H" #\Escape #\Escape) (force-output))
  #+unix (sb-ext:run-program "/usr/bin/env" '("clear") :input nil :output *standard-output*))

(defun hanoi (n from to aux)
  "Algoritmo recursivo de Torres de Hanoi, solo moviendo los discos más pequeños."
  (when (> n 0)
    (hanoi (- n 1) from aux to)
    (when (and (can-move-disk? from)
               (or (null (get-smallest-disk aux))
                   (<= (get-smallest-disk from) (get-smallest-disk aux))))
      (move-disk from to))
    (hanoi (- n 1) aux to from)))

(defun solve-hanoi (n)
  "Inicializa y resuelve el problema de las Torres de Hanoi."
  (if (> n *max-disks*)
      (format t "¡Máximo de 5 discos permitidos!~%")
      (progn
        (initialize-towers n)
        (hanoi n 'A 'C 'B))))

(solve-hanoi 4)
