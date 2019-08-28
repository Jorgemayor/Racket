# Enunciado taller 3

Diseñe un interpretador para la siguiente gramática:

Valores denotados: Texto + Número
Valores expresado: Texto + Número

<programa> := (un-programa) <expresion>

<expresion> := (numero-lit) <número>
            := (texto-lit)"<letras>"
            := (primitiva-exp) <primitiva> [expresion (expresion*) (;)]

<primitiva> := (suma) +
            := (resta) -
            := (div) /
            := (multiplicacion) *
            := (concat) concat
            := (length) length

Tenga en cuenta que:

<número> digito digito*
<letras> letra (letra | digito)*


## Ejemplos:
+[1;2;3;4] debe retornar 10
-[3;4;5] debe retornar 4 que representa (- 3 (- 4 5))
-[3;4;5;6] debe retornar -2 que representa (- 3 (- 4 (- 5 6)))
-[3;4;5;6;3] debe retornar 1 que representa (- 3 (- 4 (- 5 (- 6 3))))
+[1;2;-[3;4;5];4]  debe retornar 11
+[1;2;-[3;4;+[5;6]];4] debe retornar 17
*[3;4;5] debe retornar 60
/[3;4;5] debe retornar 15/4 o su equivalente fracción mixta 3(3/4) tras la evaluación (/ 3 (/ 4 5))
/[3;4;5;7] debe retornar 15/28 tras la evaluación (/ 3 (/ 4 (/ 5 7)))
"hola" retorna "hola"
"x" retorna "x"
hola retorna un error
x retorna un error
concat["Hola"; "mundo"; "FLP"] debe retornar "HolamundoFLP"
length["Hola"] debe retornar 4
length["Hola"; " mundo"; "FLP"] debe retornar 4, la longitud de la primer cadena


## Aclaraciones:
- Los textos se toman entre comillas (las comillas trabájelas en la gramática). Captúrelas en la representación léxica como string.
