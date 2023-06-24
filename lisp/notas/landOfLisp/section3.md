# LISP ES HACKING

## Ciclando con el comando LOOP

### El Macro loop

Cualquier ciclo que pueda desearse en un programa puede ser conseguido con el `loop` macro. Aquí un ejemplo:

    > (loop for i
            below 5
            sum i)
    10

Este suma los números positivos menores a 5, como

    0 + 1 + 2 + 3 + 4 = 10

Vemos que este ciclo no se comporta como usualmente lo 
hace un programa de lisp.

+ `for` te permite declarar una variable que itera a traves de un rango de valores. Por defecto, contará los enteros empezando por cero.

+ `below` indica que hay que parar una vez alcanzando cierto valor, excluyendo al valor mismo.

### Trucos para el loop

Se pueden usar las clausulas `from` y `to` para hacer un ciclo for que cuentra entre un rango especifico de enteros:

    > (loop for i
            from 5
            to 10
            sum i)
    45

Tambíen se puede iterar sobre valores de una lista usando `in`:

    > (loop for i
            in '(100 20 3)
            sum i)
    123

Se pueden hacer más cosas que sumar, para ello usamos la clausula `do`:
    
    > (loop for i
            below 5
            do (print i))
    0
    1
    2
    3
    4

Tambíen se puede solo realizar acciones cuando la variable (usualmente i) cumple cierta propiedad:

    > (loop for i
            below 10
          when(oddp i)
          aum i)
    25

Podemos juntar todo lo anterior para el siguiente loop:

    > (loop for i
            from 0
           do (print i)
           when (= i 5)
           return 'falafel)
    0
    1
    2
    3
    4
    5

    FALAFEL

Notemos que no hay algo que le diga cuando detenerse más que el usual return, de no estar nunca se detendria.

Se pueden regresar multiples elementos usando la clausula `collect`. Por ejemplo:

    > (loop for  i
            in '(2 3 4 5 6)
            collect (* i i))
    (4 9 16 25 36)

Es posible iterar sobre dos elementos al mismo tiempo, para esto basta con usar un for 2 veces, que a diferencia de un anidado incrementa el valor al mismo tiempo. Por ejemplo:

    > (loop for x below 10
            for y below 10
            collect (+ x y))
    (0 2 4 6 8 10 12 14 16 18)

Tambíen se puede hacer algo equivalente a los for anidados:

    > (loop for x below 10
        collect (loop for y below 10
                      collect (+ x y)))
    ((0 1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9 10) (2 3 4 5 6 7 8 9 10 11) (3 4 5 6 7 8 9 10 11 12) (4 5 6 7 8 9 10 11 12 13) (5 6 7 8 9 10 11 12 13 14) (6 7 8 9 10 11 12 13 14 15) (7 8 9 10 11 12 13 14 15 16) (8 9 10 11 12 13 14 15 16 17) (9 10 11 12 13 14 15 16 17 18))



