; Pequeño juego interactivo estilo rpg donde se explora una casa.


; Como se presenta en el libro "Land of Lisp" AList que corresponde
; a los nodos de la gráfica presentada.
(defparameter *nodes* '((living-room (you are in the living-room.
                            a wizard is snorling loudly on the couch.))
                        (garden (you are in a beautiful garde.
                            there is a well in front of you.))
                        (attic (you are in the attic.
                            there is a giant welding torch in the corner.))))

; Función que devuelve la descripción de la ubicación. Se toma como parametro
; la locación donde nos encontramos y la lista de nodos para de esta forma
; usando la función assoc encontrar el elemento correspondiente y con cadr
; tomamos unicamente la descripción. 
(defun describe-location (location nodes)
    (cadr (assoc location nodes)))

; Alist para definir los posibles caminos a tomar es decir las aristas
; de la gráfica original.
(defparameter *edges* '((living-room (garden west door)
                                     (attic upstairs ladder))
                        (garden (living-room east door))
                        (attic (living-room downstairs ladder))))

; Función que describe los posibles caminos a tomar. 
; Se toma como parametro la lista correspondiente a la arista,
; por ejemplo "(garden west door)".
(defun describe-path (edge)
`(there is a ,(caddr edge) going ,(cadr edge) from here.))

; Función que aplica a toda la lista de aristas la función para decribir
; y combina estas descripciones usando puntos.
(defun describe-paths (location edges)
    (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

; Definimos los objectos existentes en el juego.
(defparameter *objects* '(whiskey bucket frog chain))

; Creamos una lista donde se incluye el objeto y su respectiva ubicación.
(defparameter *object-locations* '((whiskey living-room)
                                   (bucket living-room)
                                   (chain garden)
                                   (frog garden)))

; Definimos una funcion que devuelve los elementos en la ubicación donde se encuentra
; el jugagador. Como parametros recibimos la ubicación del jugador, la lista de objetos
; y la lista de ubicaciones de los objetos.
; Para lo anterior creamos una función auxiliar at-loc-p que recibe un objeto y compara
; si a ubicación del objeto corresponde a la actual. Finalmente a toda la lista de objetos
; removemos aquellos que no están en la ubicación actual.
(defun objects-at (loc objs obj-locs)
    (labels ((at-loc-p (obj)
                (eq (cadr (assoc obj obj-locs)) loc)))
        (remove-if-not #'at-loc-p objs)))


; Función que devuelve la descripción de los objetos en cierta locación.
; Toma como parametro la locacíon, todos los objetos y la lista de objetos en la locación.
; Se crea una función auxiliar llamada describe-obj que recibe un objeto y devuelve un string
; diciendo que vimos ese objeto en el suelo. Despues se concatena la lista que da como
; resultado aplicar a cada objeto en la locación la funcíon auxiliar.
(defun describe-objects (loc objs obj-loc)
    (labels ((describe-obj (obj)
                `(you see a ,obj on the floor.)))
        (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))

; Definimos la ubicación inicial del jugador.
(defparameter *location* 'living-room)

; La función look da una descripción completa de la ubicación actual.
(defun look ()
    (append (describe-location *location* *nodes*)
            (describe-paths *location* *edges*)
            (describe-objects *location* *objects* *object-locations*)))

; Función para moverse de una ubicación a otra.
; Recibe como parametro la dirección en que se desea mover. Ej: west, upstairs.
; Definimos una variable local que es la lista que contiene a las aristas
; que como segundo argumento tienen la locación deseada.
; En caso de no estar vacia cambiamos la variable a la nueva dirección
; en caso de estarlo no es una ubicación valida.
(defun walk (direction)
    (let ((next (find direction
                    (cdr (assoc *location* *edges*))
                    :key #'cadr)))
    (if next
        (progn (setf *location* (car next))
            (look))
        '(you cannot go that way.))))

; Función para tomar un objeto.
; Recibe como parametro un objeto.
; Se comprueba si el objeto está en la locación actual, en caso de estarlo
; se añade a nuestros objetos, en otro caso se despliega un pensaje de error.
(defun pickup (object)
    (cond ((member object
                   (objects-at *location* *objects* *object-locations*))
            (push (list object 'body) *object-locations*)
            `(you are now carrying the ,object))
            (t '(you cannot get that.))))

; Función que devuelve los objetos que carga el jugador.
(defun inventory ()
    (cons 'items- (objects-at 'body *objects* *object-locations*)))

; Interfaz de texto del juego.
; La interfáz ciclará hasta que el usuario teclee "quit"
(defun game-repl ()
    (let ((cmd (game-read)))
        (unless (eq (car cmd) 'quit)
            (game-print (game-eval cmd))
            (game-repl))))

; Función que lee y trata el texto introducido por el usuario.
; Se define una variable cmd que lee un string y concatena los parentesis
; para que sea ejecutable en lisp. Posteriormente se agregan comillas a los elementos
; para que sean tratados como datos de esta forma prevenimos errores y exploits.
(defun game-read ()
    (let ((cmd (read-from-string
                    (concatenate 'string "(" (read-line) ")"))))
         (flet ((quote-it (x)
                        (list 'quote x)))
            (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

; Lista que contiene los comandos validos del juego.
(defparameter *allowed-commands* '(look walk pickup inventory))

; Función que ejecuta un comando.
; Recibe como parametro el comando a ejecutar.
; Si el comando pertenece a los permitidos por el juego este se ejecuta
; en caso de que no se manda un mensaje de error.
; Lo anterior ayuda a protegerse ante vulnerabilidades.
(defun game-eval (sexp)
    (if (member (car sexp) *allowed-commands*)
        (eval sexp)
        '(i do not know that command.)))

; Función que ajusta las mayusculas y minusculas del texto crudo del juego.
; Recibe como parametro el texto en forma de lista, un booleano que corresponde
; a si se desea respetar mayusculas y lit que corresponde a si hemos encontrado
; alguna comilla  para removerla. 
; Realiza el tratado de forma recursiva, si es un espacio continua.
; Si es un signo de puntuación se asegura de empezar la siguiente con mayuscula.
; Si es una comilla la borra y pasa al siguiente.
; Si está indicado que sea una mayuscula pasa la letra a mayuscula y continua.
; En otro caso simplemente la pasa a minuscula y continua.
(defun tweak-text (lst caps lit)
    (when lst
    (let ((item (car lst))
            (rest (cdr lst)))
    (cond ((eq item #\space) (cons item (tweak-text rest caps lit)))
          ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
          ((eq item #\") (tweak-text rest caps (not lit)))
           (lit (cons item (tweak-text rest nil lit)))
          ((or caps lit) (cons (char-upcase item) (tweak-text rest nil lit)))
          (t (cons (char-downcase item) (tweak-text rest nil nil)))))))

; Función que imprime el text de forma entendible para las personas.
; Recibe como parametro el texto a imprimir.
(defun game-print (lst)
    (princ (coerce (tweak-text (coerce (string-trim "() "
                                                     (prin1-to-string lst))
                                                     'list)
                                t
                                nil)
                   'string))
    (fresh-line))