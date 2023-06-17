;Definimos el valor minimo.
(defparameter *small* 1)
;Definimos el valor máximo.
(defparameter *big* 100)

;Definimos la función que adivinará usando
;busqueda binaria.
(defun guess-my-number ()
    (ash (+ *small* *big*) -1))


;Funciones para decir si es mayor o menor
;nuestro número al adivinado por la maquina.
(defun smaller ()
    (setf *big* (1- (guess-my-number)))
    (guess-my-number))

(defun bigger ()
    (setf *small* (1+ (guess-my-number)))
    (guess-my-number))

;Función para reiniciar el juego.
(defun start-over ()
    (defparameter *small* 1)
    (defparameter *big* 100)
    (guess-my-number))