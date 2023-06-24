(defun randval (n)
  (1+ (random (max 1 n))))

; Orc battle es un juego de pelea donde eres un caballero 
; rodeado por 12 monstruos peleando hasta a la muerte.


;Definimos las variables globales para el jugador.
(defparameter *player-halth* nil)
(defparameter *player-agility* nil)
(defparameter *player-strength* nil)

;Definimos las variables para los monstruos
(defparameter *monsters* nil)
(defparameter *monster-builders* nil)
(defparameter *monsters-num* 12)

;Función para inicializar el juego, inicializa las estadisticas
;del jugador y los ogros  para una vez el juego acabe monstrar
;al ganador
(defun orc-battle ()
    (init-monsters)
    (init-player)
    (game-loop)
    (when (player-dead)
        (princ "Has muerto. Game Over."))
    (when (monsters-dead)
        (princ "Felicidades! Acabaste con todos tus enemigos.")))

;Función que se realiza cada turno y de forma indefinida hasta
;acabar el juego.
(defun game-loop ()
    (unless (or (player-dead) (monsters-dead))
        (show-player)
        (dotimes (k (1+ (truncate (/ (max 0 *player-agility*) 15))))
         (unless (monsters-dead)
            (show-monsters)
            (player-attack)))
        (fresh-line)
        (map 'list (lambda (m)
                        (or (monster-dead m) (monster-attack m)))
                    *monsters*)
        (game-loop)))

;Función para inicializar al jugador con estadisticas base.
(defun init-player ()
    (setf *player-health* 30)
    (setf *player-agility* 30)
    (setf *player-strength* 30))

;Función que devuelve si es que el jugador está meurto
(defun player-dead ()
    (<= *player-health* 0))

;Función para mostrar el estado actual del jugador.
(defun show-player ()
    (fresh-line)
    (princ "Tu eres un legendario caballero con una salud de ")
    (princ *player-health*)
    (princ ", una agilidad de ")
    (princ *player-agility*)
    (princ ", y una fuerza de ")
    (princ *player-strength*))

;Función para atacar.
;Existen 3 tipos de ataque. 
;Estocada, golpea un valor aleatorio entre 2 y la fuerza del jugador entre 2.
;Doble balanceo golpea dos veces pero el daño es un valor aleatorio entre 0 y la fuerza del jugador.
;Frenesi tiene de daño de 1 pero ataca tantas veces un vlaor aleatorio entre la fuerza del jugador entre 3.
(defun player-attack ()
    (fresh-line)
    (princ "Elige un ataque: e[s]tocada [d]oble balanceo f[r]enesi:")
    (case (read)
        (s (monster-hit (pick-monster)
                            (+ 2 (randval (ash *player-strength* -1)))))
        (d (let ((x (randval (truncate (/ *player-strength* 6)))))
                (princ "Tu balanceo tiene una fuerza de ")
                (princ x)
                (fresh-line)
                (monster-hit (pick-monster) x)
                (unless (monsters-dead)
                    (monster-hit (pick-monster) x))))
                (otherwise (dotimes (x (1+ (randval (truncate (/ *player-strength* 3)))))
                            (unless (monsters-dead)
                                (monster-hit (random-monster) 1))))))

;Función para elegir de forma aleatoria un monstruo.
(defun random-monster ()
    (let ((m (aref *monsters* (random (length *monsters*)))))
    (if (monster-dead m)
        (random-monster)
        m)))

;Función para que el usuario elija de forma correcta al monstruo.
(defun pick-monster ()
    (fresh-line)
    (princ "Ogro #:")
    (let ((x (read)))
        (if (not (and (integerp x) (>= x 1) (<= x *monsters-num*)))
            (progn (princ "Ese no es un ogro valido.")
                   (pick-monster))
            (let ((m (aref *monsters* (1- x))))
                (if (monster-dead m)
                    (progn (princ "Ese ogro ya está muerto.")
                           (pick-monster))
                    m)))))

; Función que inicializa a los ogros
(defun init-monsters ()
    (setf *monsters*
          (map 'vector
               (lambda (x)
                    (funcall (nth (random (length *monster-builders*))
                             *monster-builders*)))
               (make-array *monsters-num*))))

;Función que devuelve si en un ogro está muerto.
(defun monster-dead (m)
    (<= (monster-health m) 0))

;Función que devuelve si todos los ogros están muertos.
(defun monsters-dead ()
    (every #'monster-dead *monsters*))

;Función para mostrar las estadisticas de los ogros.
(defun show-monsters ()
    (fresh-line)
    (princ "Tus enemigos:")
    (let ((x 0))
        (map 'list
            (lambda (m)
                (fresh-line)
                (princ "  ")
                (princ (incf x))
                (princ ". ")
                (if (monster-dead m)
                    (princ "**muerto**")
                    (progn (princ "(Salud = ")
                           (princ (monster-health m))
                           (princ ") ")
                           (monster-show m))))
            *monsters*)))


;Creamos el equivalente a una clase padre llamada monstruo.
(defstruct monster (health (randval 10)))

;Metodo que recibe un monstruo m y un entero x.
;Resta x al valor de salud del monstruo m.
(defmethod monster-hit (m x)
    (decf (monster-health m) x)
    (if (monster-dead m)
        (progn (princ "Mataste a ")
               (princ (type-of m))
               (princ "! "))
        (progn (princ "Dañaste a ")
               (princ (type-of m))
               (princ " quitando ")
               (princ x)
               (princ " puntos de salud! "))))

(defmethod monster-show (m)
    (princ "Feroz ")
    (princ (type-of m)))

(defmethod monster-attack (m))

(defstruct (orc (:include monster)) (club-level (randval 8)))
(push #'make-orc *monster-builders*)

(defmethod monster-show ((m orc))
    (princ "Un malvado Ogro con nivel ")
    (princ (orc-club-level m))
    (princ " de garrote"))

(defmethod monster-attack ((m orc))
    (let ((x (randval (orc-club-level m))))
        (princ "Un ogro balancea su garrote hacia ti y golpea ")
        (princ x)
        (princ " de tus puntos de vida. ")
        (decf *player-health* x)))

(defstruct (hydra (:include monster)))
(push #'make-hydra *monster-builders*)

(defmethod monster-show ((m hydra))
    (princ "Una maliciosa Hydra con ")
    (princ (monster-health m))
    (princ " cabezas."))

(defmethod monster-hit ((m hydra) x)
    (decf (monster-health m) x)
    (if (monster-dead m)
        (princ "El cuerpo de la hydra completamente decapitada cae al suelo!")
        (progn (princ "Tu cortaste ")
               (princ x)
               (princ " de las cabezas de la hydra! "))))

(defmethod monster-attack ((m hydra))
    (let ((x (randval (ash (monster-health m) -1))))
        (princ "Una hydra te ataca con ")
        (princ x)
        (princ " de sus cabezas! Y le acaba de crecer una nueva cabeza! ")
        (incf (monster-health m))
        (decf *player-health* x)))

(defstruct (slime-mold (:include monster)) (sliminess (randval 5)))
(push #'make-slime-mold *monster-builders*)

(defmethod monster-show ((m slime-mold))
    (princ "Un slime con una viscosidad de ")
    (princ (slime-mold-sliminess m)))

(defmethod monster-attack ((m slime-mold))
    (let ((x (randval (slime-mold-sliminess m))))
    (princ "Un trozo del slime se enredada alrededor de tus piernas y disminuye tu agilidad en ")
    (princ x)
    (princ "! ")
    (decf *player-agility* x)
    (when (zerop (random 2))
        (princ "Tambien salpica tu cara, llevandose un punto de vida! ")
        (decf *player-health*))))

(defstruct (brigand (:include monster)))
(push #'make-brigand *monster-builders*)

(defmethod monster-attack ((m brigand))
    (let ((x (max *player-health* *player-agility* *player-strength*)))
        (cond ((= x *player-health*)
            (princ "")
            (decf *player-health* 2))
            ((= x *player-agility*)
             (princ "¡Un bandido te golpea con su honda y te quita 2 puntos de salud! ")
             (decf *player-agility* 2))
            ((= x *player-strength*)
             (princ "¡Un bandido te corta el brazo con su látigo, quitándote 2 puntos de fuerza! ")
             (decf *player-strength* 2)))))