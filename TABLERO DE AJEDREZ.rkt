#lang racket

;Aqui solamente creamos la interfaz ajena al tablero
(require graphics/graphics)
(open-graphics)
;definimos chess como el viewport principal del juego
(define chess (open-viewport "Ajedrez" 600 400))
((draw-viewport chess) "brown")
((draw-string chess) (make-posn 470 70) "AJEDREZ" "snow")
((draw-string chess) (make-posn 478 120) "Juegan:" "snow")
((draw-solid-rectangle chess) (make-posn 440 350) 120 20 "gray")
((draw-solid-rectangle chess) (make-posn 440 350) 118 18 "white")
((draw-string chess) (make-posn 455 364) "Terminar juego" "black")

;aqui definimos los nombres de las fichas por comodidad respectivamente
(define torreBlanco "Torre blanco.png") ;torre color blanco
(define caballoBlanco "Caballo blanco.png") ;caballo color blanco
(define alfilBlanco "Alfil blanco.png") ;alfil color blanco
(define reyBlanco "Rey blanco.png") ;rey color blanco
(define reinaBlanco "Reina blanco.png") ;reina color blanco
(define peonBlanco "Peon blanco.png") ;peon color blanco
(define torreNegro "Torre negro.png") ;torre color negro
(define caballoNegro "Caballo negro.png") ;caballo color negro
(define alfilNegro "Alfil negro.png") ;alfil color negro
(define reyNegro "Rey negro.png");rey color negro
(define reinaNegro "Reina negro.png");reina color negro
(define peonNegro "Peon negro.png");peon color negro

;asociamos una imagen a cada caracter por usar
;char sera el caracter que evaluaremos
(define (EvaluateImage char)
  (if (equal? char #\T)
      torreBlanco
      (if (equal? char #\C)
          caballoBlanco
          (if (equal? char #\A)
              alfilBlanco
              (if (equal? char #\R)
                  reyBlanco
                  (if (equal? char #\D)
                      reinaBlanco
                      (if (equal? char #\P)
                          peonBlanco
                          (if (equal? char #\t)
                              torreNegro
                              (if (equal? char #\c)
                                  caballoNegro
                                  (if (equal? char #\a)
                                      alfilNegro
                                      (if (equal? char #\r)
                                          reyNegro
                                          (if (equal? char #\d)
                                              reinaNegro
                                              (if (equal? char #\p)
                                                  peonNegro
                                                  (void));end if char p
                                              );end if char d
                                          );end if char r
                                      );end if char a
                                  );end if char c
                              );end if char t
                          );end if char P
                      );end if char D
                  );end if char R
              );end if char A
          );end if char C
      );end if char T
  );end define EvaluateImage

;La funcion PieceForDisplay pondrá todas las piezas en orden dependiendo de la string dada
;string será la string a mostrar, counter será un contador y viewport será el viewport en el que lo dibujemos
(define (PieceForDisplay string counter viewport)
  (if (< counter (string-length string))
      ;si recibe el caracter z no pondrá ninguna imagen
      (if (equal? (string-ref string counter) #\z)
          (PieceForDisplay string (+ counter 1) viewport)
          ;si recibe un caracter distinto a z, evaluará la imagen a colocar y la pondrá en su respectiva posición
          (begin
            (((draw-pixmap-posn (EvaluateImage (string-ref string counter))) viewport) (make-posn (* (remainder counter 8) 50) (* 50 (quotient counter 8))))
            (PieceForDisplay string (+ counter 1) viewport)
            );end begin
          );end if equal? string-ref #\z
      ;else,
      (void)
      );end if < counter string-length
  );end define PieceForDisplay
;La funcion DrawBoard pondrá una tabla por encima de las piezas del turno anterior
;string será la string con la cual llamaremos a PieceForDisplay
(define (DrawBoard string)
  ((draw-pixmap chess) "Tablero 8x8.png" (make-posn 0 0))
  (PieceForDisplay string 0 chess)
  );end define DrawBoard
  
;CheckTurn va a llamar al siguiente turno. si recibe playerTurn=0, el siguiente turno será de las blancas; de lo contrario,
;si recibe playerTurn=1, el siguiente turno será de las negras.
;arrastramos tambien string ya que la necesitaremos en cualquiera de ambas funciones que llamemos
(define (CheckTurn playerTurn string)
  (if (equal? playerTurn 1)
      (WhiteTurn string)
      (BlackTurn string)
      );end if playerTurn=0
  );end define CheckTurn

;la funcion PieceColor? revisará si la pieza es de color blanco, negro o si no hay ninguna pieza.
;string sera la string a evaluar e index será el subindice de la string que evaluaremos
(define (PieceColor? string index)
  ;si la pieza es blanca (mayúscula) devolverá 1
  (if (or
       (equal? (string-ref string index) #\P) (equal? (string-ref string index) #\T) (equal? (string-ref string index) #\C)
       (equal? (string-ref string index) #\A) (equal? (string-ref string index) #\R) (equal? (string-ref string index) #\D))
      1
      ;si la pieza es negra (minúscula) devolverá -1
      (if (or
           (equal? (string-ref string index) #\p) (equal? (string-ref string index) #\t) (equal? (string-ref string index) #\c)
           (equal? (string-ref string index) #\a) (equal? (string-ref string index) #\r) (equal? (string-ref string index) #\d))
          -1
          ;si no es ninguna, devolverá 0
          0
          );end if... -1
      );end if... 1
  );end define PieceColor?

;servirá para crear una string nueva la cual indicará las posiciones del ajedrez
;char es el caracter a mover
;oldString es la string de la posicion anterior al movimiento
;prevPos es la posicion anterior de la pieza
;nextPos es la posicion a la cual se va a mover
(define (MakeNewString char oldString prevPos nextPos)
  ;convertimos char a string
  (define newString (string char))
  ;si recibe dos posiciones iguales, no hace nada, para evitar detener el flujo del programa
  (if (or (= nextPos prevPos) (> nextPos 63) (> prevPos 63))
      (void)
      ;else,
      (if (> nextPos prevPos)
          (string-append (substring oldString 0 prevPos) "z" (substring oldString (+ prevPos 1) nextPos) (substring newString 0 1) (substring oldString (+ nextPos 1)))
          (string-append (substring oldString 0 nextPos) (substring newString 0 1) (substring oldString (+ nextPos 1) prevPos) "z" (substring oldString (+ prevPos 1)))
          );end if > nextPos prevPos
      );end if = nextPos prevPos
  );end define MakeNewString

;funciones para la comodidad de encontrar casillas
;pos-x es una nueva funcion para definir en que columna está la casilla
(define (position-x click) (quotient (posn-x click) 50))
;similarmente, pos-y indica en que fila está la casilla
(define (position-y click) (quotient (posn-y click) 50))
;square unirá dos valores y creará un subindice para la casilla
(define (square a b) (+ a (* b 8)))

;definimos la funcion para la coronacion de peones
(define (Coronation strBoard position pieceColor)
  ;damos un mensaje para que el jugador sepa que debe esperar para escoger su pieza
         ((draw-rectangle chess) (make-posn 440 180) 120 40 "black")
         ((draw-string chess) (make-posn 446 206) "CORONACION" "white")
  ;creamos un nuevo viewport llamado coronationWindow el cual se abrirá cuando el jugador desee coronar una pieza
  (define coronationWindow (open-viewport "Escoja su ficha" 200 50))
  ;definimos una funcion para crear una string nueva, la cual será la siguiente posición del tablero
  ;le damos una string, una casilla y un caracter (con los parametros str, square y char respectivamente)
  (define (CoronationString str square char)
    (string-append (substring str 0 square) (string char) (substring str (+ square 1)))
    );end CoronationString
  ;ChoosePiece le pide un click al usuario y dependiendo del color del peon a coronar,
  ;indicado por el parametro color, crea en el tablero la nueva posición
  (define (ChoosePiece color)
    ;definimos click como la posicion en horizontal de 0 a 3 entre las 4 piezas del click que da el usuario,
    ; y la usamos luego para escoger con cual pieza reemplazaremos el peon
    (define click (position-x (mouse-click-posn (get-mouse-click coronationWindow))))
    ;si el color es 1, usará las piezas con caracteres de letra mayuscula (los cuales representan piezas blancas)
    (if (= color 1)
        (begin (close-viewport coronationWindow) ((draw-solid-rectangle chess) (make-posn 440 180) 120 40 "brown")
               (DrawBoard (CoronationString strBoard position (string-ref "ACTD" click)))
               (CheckTurn (* pieceColor -1) (CoronationString strBoard position (string-ref "ACTD" click)))
               );end begin
        ;si el color no es 1, usará las piezas con caracteres de letra minuscula (los cuales representan piezas negras)
        (begin (close-viewport coronationWindow) ((draw-solid-rectangle chess) (make-posn 440 180) 120 40 "brown")
               (DrawBoard (CoronationString strBoard position (string-ref "actd" click)))
               (CheckTurn (* pieceColor -1) (CoronationString strBoard position (string-ref "actd" click)))
               );end begin
        );end if = color 1
    );end define ChoosePiece
  ;revisa si el color de la pieza que le dimos es blanco. Si lo es, dibuja un recuadro de color negro para que haga de fondo a las piezas
  ;si no es blanco, simplemente dibujará las piezas, sin ningun tipo de fondo detrás de ellas
  (if (= pieceColor 1)
      (begin ((draw-solid-rectangle coronationWindow) (make-posn 0 0) 200 50 "brown") (PieceForDisplay "ACTD" 0 coronationWindow));end begin
      (PieceForDisplay "actd" 0 coronationWindow)
      );end if = pieceColor 1
  (ChoosePiece pieceColor)
  );end define Coronation
  
;esta es la funcion que genera el movimiento, y engloba a todas las funciones de movimiento de cada pieza
;string será la string del turno anterior
;column será una position-x
;row será una position-y
;newPos será un click de mouse, con el cual crearemos la casilla de la siguiente posicion
;currentTurn indica el turno actual, blancas o negras
(define (MovePiece string column row newPos currentTurn)
  ;definimos las posiciones de casillas clickeadas como identificadores newPos-x y newPos-y
  (define newPos-x (position-x newPos))
  (define newPos-y (position-y newPos))
  ;definimos como oldSquare la casilla donde estaba la ficha originalmente
  (define oldSquare (square column row))
  ;definimos como newSquare la casilla a la que queremos moverla
  (define newSquare (square newPos-x newPos-y))
  ;definimos como madeMove la string para la posicion del siguiente turno, con el movimiento que deseamos realizar ya efectuado
  (define madeMove (MakeNewString (string-ref string oldSquare) string oldSquare newSquare))
  ;revisamos si el segundo click está después de los limites del tablero
   (if (> newPos-x 7)
       (CheckTurn currentTurn string)
       (void))
  ;InvalidMove creará una casilla que indica que el movimiento jugado fue inválido
  (define (InvalidMove)
    ((draw-solid-rectangle chess) (make-posn 440 180) 120 40 "red")
    ((draw-string chess) (make-posn 479 198) "Jugada" "white")
    ((draw-string chess) (make-posn 477 214) "inválida!" "white")
    );end define InvalidMove

   ;InvalidMoveCheck creará una casilla que indica que el movimiento jugado fue inválido porque el jugador está en jaque
  (define (InvalidMoveCheck)
    ((draw-solid-rectangle chess) (make-posn 440 180) 120 40 "red")
    ((draw-string chess) (make-posn 479 198) "Sigues" "white")
    ((draw-string chess) (make-posn 473 212) "en jaque!" "white")
    );end define InvalidMoveCheck
       
  ;revisa si una casilla está diagonal a otra, si lo está devuelve la distancia en la string entre las casillas
  ;pos es un contador, a es la casilla inicial y b es la casilla destino
  (define (Diagonal? pos a b)
        ;si el contador pos llega a cero, devolvemos 0, lo cual significa que la casilla no está diagonal a la otra
        (if (equal? pos 0) 0
            ;si alguno de estos casos se cumple, devuelve pos*9 o pos*7 dependiendo de la dirección
            (if (and (equal? b (+ a (* pos 9)))  (not (>= column newPos-x))) (* pos 9)  ;devuelve pos*9
            (if (and (equal? b (+ a (* pos 7)))  (not (<= column newPos-x))) (* pos 7)  ;devuelve pos*7
            (if (and (equal? b (+ a (* pos -9))) (not (<= column newPos-x))) (* pos -9) ;devuelve pos*-9
            (if (and (equal? b (+ a (* pos -7))) (not (>= column newPos-x))) (* pos -7) ;devuelve pos*-7
                ;si ninguno se cumple reducimos pos y revisamos de nuevo
                (Diagonal? (- pos 1) a b)
            );end if * pos -7
            );end if * pos -9
            );end if * pos 7
            );end if * pos 9
            );end if equal pos 0
    );end define Diagonal?

  ;Indica la dirección en la que está una casilla respecto a otra
  ;pos es un contador, a es la casilla inicial y b la casilla final
  (define (DiagonalDirection? pos a b)
    ;si a es mayor que b, invierte sus posiciones
        ;si el contador pos llega a cero, devolvemos 0, lo cual significa que la casilla no está diagonal a la otra
        (if (equal? pos 0) 0
            ;si alguno de estos casos se cumple, devuelve 9 o 7 dependiendo de la dirección
            (if (equal? b (+ a (* pos 9))) 9 ;devuelve 9, indicando que está a un multiplo de 9 de distancia en la string
            (if (equal? b (+ a (* pos 7))) 7 ;devuelve 7, indicando que está a un multiplo de 7 de distancia en la string
            (if (equal? a (+ b (* pos 9))) -9 ;devuelve 9, indicando que está a un multiplo de 9 de distancia en la string
            (if (equal? a (+ b (* pos 7))) -7 ;devuelve 7, indicando que está a un multiplo de 7 de distancia en la string
                ;si ninguno se cumple, reducimos pos y evaluamos de nuevo
                (DiagonalDirection? (- pos 1) a b)
                );end if * pos -7
                );end if * pos -9
                );end if * pos 7
                );end if * pos 9
            );end if equal pos 0
    );end define DiagonalDirection?

  ;revisa si una casilla está horizontal o vertical a otra, si lo está devuelve la distancia en la string entre las casillas
  ;pos es un contador, a es la casilla inicial y b es la casilla destino
  (define (Lateral? pos a b)
        ;si el contador pos llega a cero, devolvemos 0, lo cual significa que la casilla no está lateral a la otra
        (if (equal? pos 0) 0
            ;si se cumplen alguno de estos casos, devuelve pos*8 o pos, dependiendo de la orientación
            (if (and (equal? b (+ a (* pos 8))) (= column newPos-x)) (* pos 8) ;indica que se mueve en vertical
            (if (and (equal? b (+ a pos))       (= row newPos-y))       pos    ;indica que se mueve en horizontal
            (if (and (equal? a (+ b (* pos 8))) (= column newPos-x)) (* pos -8);indica que se mueve en vertical
            (if (and (equal? a (+ b pos))       (= row newPos-y))    (- pos)   ;indica que se mueve en horizontal
                ;si ninguno se cumple, reducimos pos y evaluamos de nuevo
                (Lateral? (- pos 1) a b)
                );end if equal... -pos
            );end if equal... * pos -8
            );end if equal... pos
            );end if equal... * pos 8
            );end if equal pos 0
    );end define Lateral?

  ;revisa si una casilla está horizontal o vertical a otra, si lo está devuelve la direccion en la que está
  ;pos es un contador, a es la casilla inicial y b es la casilla destino
  (define (LateralDirection? pos a b)
        ;si el contador pos llega a cero, devolvemos 0, lo cual significa que la casilla no está lateral a la otra
        (if (equal? pos 0) 0
            ;si se cumplen alguno de estos casos, devuelve 8 o pos, dependiendo de la orientación
            (if (equal? b (+ a (* pos 8))) 8 ;devuelve 8, indicando que se mueve en vertical
            (if (equal? b (+ a pos)) pos ;devuelve pos, indicando a cuantas casillas está hacia la izquierda o derecha
            (if (equal? a (+ b (* pos 8))) -8 ;devuelve 8, indicando que se mueve en vertical
            (if (equal? a (+ b pos)) (- pos) ;devuelve pos, indicando a cuantas casillas está hacia la izquierda o derecha
                ;si ninguno se cumple, reducimos pos y evaluamos de nuevo
                (LateralDirection? (- pos 1) a b)
                );end if equal... -pos
                );end if equal... * pos -8           
                );end if equal... pos
                );end if equal... * pos 8
            );end if equal pos 0
    );end define LateralDirection?

  ;revisa si una casilla está en L hacia otra casilla (movimiento del caballo)
  (define (LShape? a b)
    ;si a es menor que b, se invierten los valores
    (if (< a b) (LShape? b a)
        ;revisa si una casilla está en direccion de L a otra
        (if (or (equal? a (+ b 6)) (equal? a (+ b 10))
                (equal? a (+ b 15)) (equal? a (+ b 17)));end or
            1 ;devuelve 1, lo que significa que se cumple
            0 ;si no, devuelve 0, lo que significa que no se cumple
            );end if or
        );end if < a b
    );end define LShape?

  ;Revisa si una casilla está en jaque
  (define (OnCheck? str square kingColor)
    ;revisa si una casilla está en un jaque de caballo
    (define (KnightCheck? value)
      (if (or (> (+ value square) 63) (< (+ value square) 0))
          (void)
      (if (= kingColor 1)
      (equal? (string-ref str (+ square value)) #\c)
      (equal? (string-ref str (+ square value)) #\C)
      );end if = kingColor 1
      );end if > (+ value square) 63
     );end define KnightCheck?
    (define (DiagonalCheck? distance direction)
      (if (= distance 8)
                   #f
          (if (= kingColor 1)
              (if (or (> (+ square (* distance direction)) 63)
                      (< (+ (* distance direction)) 0))
                  #f
            (if (and
                 (or
                     (equal? (string-ref str (+ square (* distance direction))) #\a)
                     (equal? (string-ref str (+ square (* distance direction))) #\d))
                 (equal? (Restrictions str (- (* distance direction) direction) direction square (+ square (* distance direction))) 0));end and
                #t
            (DiagonalCheck? (+ distance 1) direction)
            );end if equal? string-ref
            );end if or >...
          ;else (= kingColor -1),
              (if (or (> (+ square (* distance direction)) 63) (< (+ (* distance direction)) 0))
                  #f
            (if (and
                 (or (equal? (string-ref str (+ square (* distance direction))) #\A) (equal? (string-ref str (+ square (* distance direction))) #\D))
                 (equal? (Restrictions str (- (* distance direction) direction) direction square (+ square (* distance direction))) 0))
                #t
            (DiagonalCheck? (+ distance 1) direction)
            );end if equal? string-ref
            );end if or >...
          );end if = kingColor 1
          );end if = distance 8
      );end define DiagonalCheck?
    (define (LateralCheck? distance direction)
      (if (= distance 8)
              #f
          (if (= kingColor 1)
              (if (or
                   (> (+ square (* distance direction)) 63)
                   (< (+ (* distance direction)) 0))
                  #f
            (if (and
                 (or (equal? (string-ref str (+ (* distance direction) square)) #\t)
                     (equal? (string-ref str (+ (* distance direction) square)) #\d))
                 (equal? (Restrictions str (- (* direction distance) direction) direction square (+ square (* distance direction))) 0))
                #t
            (LateralCheck? (+ distance 1) direction)
            );end if equal? string-ref
            );end if or >...
          ;else (= kingColor -1),
              (if (or (> (+ square (* distance direction)) 63) (< (+ (* distance direction)) 0))
                  #f
            (if (and
                 (or (equal? (string-ref str (+ (* distance direction) square)) #\T)
                     (equal? (string-ref str (+ (* distance direction) square)) #\D))
                 (equal? (Restrictions str (- (* direction distance) direction) direction square (+ square (* distance direction))) 0))
                #t
            (LateralCheck? (+ distance 1) direction)
            );end if equal? string-ref
            );end if or >...
          );end if = kingColor 1
          );end if = distance 8
      );end define LateralCheck?
    (define (PawnCheck?)
      (if (= kingColor 1)
          (if (or (< (- square 7) 0) (< (- square 9) 0)) #f
          (if (or
               (equal? (string-ref str (- square 7)) #\p)
               (equal? (string-ref str (- square 9)) #\p)
               ) #t #f);end if or equal? string-ref...
          );end if or <...
          (if (or (> (+ square 7) 63) (> (+ square 9) 63)) #f
          (if (or
               (equal? (string-ref str (+ square 7)) #\P)
               (equal? (string-ref str (+ square 9)) #\P)
               ) #t #f);end if or equal? string-ref...
          );end if or <...
          );end if = kingColor 1
      );end define PawnCheck?
    (if (or
         (equal? (KnightCheck? -6) #t)
         (equal? (KnightCheck? -10) #t)
         (equal? (KnightCheck? -15) #t)
         (equal? (KnightCheck? -17) #t)
         (equal? (KnightCheck? 6) #t)
         (equal? (KnightCheck? 10) #t)
         (equal? (KnightCheck? 15) #t)
         (equal? (KnightCheck? 17) #t)    
         (equal? (DiagonalCheck? 1 -9) #t)
         (equal? (DiagonalCheck? 1 -7) #t)
         (equal? (DiagonalCheck? 1 7) #t)
         (equal? (DiagonalCheck? 1 9) #t)       
         (equal? (LateralCheck? 1 -8) #t)       
         (equal? (LateralCheck? 1 -1) #t)       
         (equal? (LateralCheck? 1 1) #t)       
         (equal? (LateralCheck? 1 8) #t)
         (equal? (PawnCheck?) #t));end or
        #t #f)
    );end OnCheck?
  
  ;esta funcion rervisará si hay una pieza entre una casilla y otra, lo cual usaremos
  ;para que los alfiles, torres y reinas no puedan saltarse otras fichas
  ;string será la string a revisar, pos será un contador, direction será la dirección en la que
  ;está una casilla en relación a otra, a y b serán casillas (subindices de la string)
  (define (Restrictions string pos direction a b)
    ;si pos llega a cero devuelve cero, lo cual significaría que no hay restriccion
    (if (or (= pos 0) (= direction 0)) 0   
            ;Revisa recursivamente si estan vacias las casillas entre las posiciones dadas
            (if (equal? (string-ref string (+ a pos)) #\z)
                (Restrictions string (- pos direction) direction a b) 1) ;end if equal? string-ref
            );end if = pos 0
    );end define Restrictions

  (define (FindKing str pos kingColor)
    (if (= pos 64) (void)
        (if (equal? kingColor 1) 
            (if (equal? (string-ref str pos) #\R) pos
                (FindKing str (+ pos 1) kingColor)
                );end if equal? string-ref... #\R
            (if (equal? (string-ref str pos) #\r) pos
                (FindKing str (+ pos 1) kingColor)
                );end if equal? string-ref... #\r
            );end if equal? kingColor 1
    );end if = pos 64
    );end define FindKing

  ;revisa si el movimiento realizado deja en jaque al oponente
(define (GiveCheck? str kingColor)
  (if (equal? (OnCheck? str (FindKing str 0 kingColor) kingColor) #t)
      #t #f))
  (define (MoveOnCheck)
    ((draw-rectangle chess) (make-posn 440 180) 120 40 "black")
    ((draw-string chess) (make-posn 475 206) "JAQUE" "white"))

    (if (GiveCheck? madeMove currentTurn)
      (begin (InvalidMoveCheck) (CheckTurn currentTurn string))
      (void))
  (if (GiveCheck? madeMove (* currentTurn -1))
      (MoveOnCheck)
      (void))
  
  ;Movimiento de los peones blancos. Se separa de los peones negros debido a la dirección de avance de cada uno de ellos.
  (define (WhitePawn)
    ;revisa si está en la fila 7 del tablero (la cual es nombrada fila 6 en la string), para permitir el doble avance del peón
    (if (and (= row 6) (= oldSquare (+ newSquare 16)) (equal? (string-ref string newSquare) #\z))
        (begin (DrawBoard madeMove) (CheckTurn -1 madeMove))
        (begin
          ;si no está en la fila 7, revisa si la casilla seleccionada está justo adelante del peón, para permitir su avance
          (if (and (= oldSquare (+ newSquare 8)) (equal? (string-ref string newSquare) #\z))
              ;aqui revisa esi el movimiento dejo al peon en una casilla de coronacion
              (if (= newPos-y 0)
                  (begin (DrawBoard madeMove) (Coronation madeMove newSquare 1))
                  (begin (DrawBoard madeMove) (CheckTurn -1 madeMove))
                  );end if = * 8 newPos-y 0
              ;si no se cumple, revisa si está en diagonal al peón y hay otra pieza allí, para permitir capturarla
              (if (and (not (equal? (string-ref string newSquare) #\z))
                       (= (PieceColor? string newSquare) -1)
                       (or  (= oldSquare (+ newSquare 7))
                            (= oldSquare (+ newSquare 9))) ;end or
                       ) ;end and
                  ;aqui revisa si la captura dejó al peón en una casilla de coronación
                  (if (= newPos-y 0)
                      (begin (DrawBoard madeMove) (Coronation madeMove newPos-x 1))
                      (begin (DrawBoard madeMove) (CheckTurn -1 madeMove))
                      );end if = * 8 newPos-y 0
                  ;si ninguno de estos casos se cumple, se reiniciará el turno con CheckTurn
                  (begin (InvalidMove) (CheckTurn currentTurn string))
                  );end if and or (captura diagonal)
              );end if (avance de una casilla)
          );end begin
        );end if (avance de doble casilla)
    );end define WhitePawn
  ;Movimiento de los peones negros. Se separa de los peones blancos debido a la dirección de avance de cada uno de ellos.
  (define (BlackPawn)
    ;revisa si está en la fila 2 del tablero (la cual es nombrada fila 1 en la string), para permitir el doble avance del peón
    (if (and (= row 1) (= oldSquare (- newSquare 16)) (equal? (string-ref string newSquare) #\z)) 
        (begin (DrawBoard madeMove) (CheckTurn 1 madeMove))
        (begin
          ;si no está en la fila 2, revisa si la casilla seleccionada está justo adelante del peón, para permitir su avance
          (if (and (= oldSquare (- newSquare 8)) (equal? (string-ref string newSquare) #\z))
              ;aqui revisa si el movimiento dejó al peón en una casilla de coronación
              (if (= newPos-y 7)
                  (begin (DrawBoard madeMove) (Coronation madeMove newSquare -1))
                  (begin (DrawBoard madeMove) (CheckTurn 1 madeMove))
                  );end if = * 8 newPos-y 0
              (if (and ;si no se cumple, revisa si está en diagonal al peón y hay otra pieza allí, para permitir capturarla
                   (or (= oldSquare (- newSquare 7))
                       (= oldSquare (- newSquare 9)))
                   (not (equal? (string-ref string newSquare) #\z))
                   (= (PieceColor? string newSquare) 1))
                  ;aqui revisa si la captura dejó al peón en una casilla de coronación
                  (if (= newPos-y 7)
                      (begin (DrawBoard madeMove) (Coronation madeMove newSquare -1))
                      (begin (DrawBoard madeMove) (CheckTurn 1 madeMove))
                      );end if = * 8 newPos-y 0
                  ;si ninguno de estos casos se cumple, se reiniciará el turno con CheckTurn
                  (begin (InvalidMove) (CheckTurn currentTurn string)))))))
  ;Movimiento de los alfiles
  (define (Bishop)
    ;revisa si la casilla está en diagonal y no tiene una pieca del mismo color. Si no, se reiniciará el turno con CheckTurn    
    (define distance (Diagonal? 7 oldSquare newSquare))
    (define direction (DiagonalDirection? 7 oldSquare newSquare))
    (if (and (not (= distance 0)) (equal? (Restrictions string (- distance direction) direction oldSquare newSquare) 0)
             (not (equal? (PieceColor? string newSquare) currentTurn)))
        (begin (DrawBoard madeMove) (CheckTurn (* currentTurn -1) madeMove))
        ;else,
        (begin (InvalidMove) (CheckTurn currentTurn string))
        );end if Diagonal?
    );end define Bishop
  (define (Knight)
    ;revisa si la casilla está en diagonal y no tiene una pieca del mismo color. Si no, se reiniciará el turno con CheckTurn
    (if (and (= (LShape? oldSquare newSquare) 1) (not (equal? (PieceColor? string newSquare) currentTurn)))
        (begin (DrawBoard madeMove) (CheckTurn (* currentTurn -1) madeMove))
        (begin (InvalidMove) (CheckTurn currentTurn string))
        );end if LShape?
    );end define Knight
  ;Movimiento de las torres
  (define (Tower)
    (define distance (Lateral? 7 oldSquare newSquare))
    (define direction (LateralDirection? 7 oldSquare newSquare))
    ;revisa si la casilla está hacia alguno de los lados y no tiene una pieza del mismo color. Si no, se reiniciará el turno con CheckTurn
    (if (and (not (= distance 0)) (equal? (Restrictions string (- distance direction) direction oldSquare newSquare) 0)
             (not (equal? (PieceColor? string newSquare) currentTurn)))
        (begin (DrawBoard madeMove) (CheckTurn (* currentTurn -1) madeMove))
        (begin (InvalidMove) (CheckTurn currentTurn string)));end if Lateral?
    );end define Tower
  ;Movimiento de la reina
  (define (Queen)
    (define lateralDistance (Lateral? 7 oldSquare newSquare))
    (define lateralDirection (LateralDirection? 7 oldSquare newSquare))
    (define diagonalDistance (Diagonal? 7 oldSquare newSquare))
    (define diagonalDirection (DiagonalDirection? 7 oldSquare newSquare))
    ;revisa si la casilla está hacia los lados o en diagonal y no tiene una pieza del mismo color. Si no, se reiniciará el turno con CheckTurn
    (if (and (or
              (and (not (= lateralDistance 0)) (equal? (Restrictions string (- lateralDistance lateralDirection) lateralDirection oldSquare newSquare) 0))
              (and (not (= diagonalDistance 0)) (equal? (Restrictions string (- diagonalDistance diagonalDirection) diagonalDirection oldSquare newSquare) 0))
              );end or
             (not (equal? (PieceColor? string newSquare) currentTurn)))
                  (begin (DrawBoard madeMove)
                  (CheckTurn (* currentTurn -1) madeMove))
        (begin (InvalidMove) (CheckTurn currentTurn string))
        );end if and Lateral? Diagonal? PieceColor?
    );end define Queen
  ;Movimiento del rey
  (define (King)
    ;revisa si la casilla está hacia los lados o en diagonal a una sola casilla de distancia y no tiene una pieza del mismo color. Si no, se reiniciará el turno con CheckTurn
    (if (and
         (or (not (= (Lateral? 1 oldSquare newSquare) 0)) (not (= (Diagonal? 1 oldSquare newSquare) 0)))
         (not (equal? (PieceColor? string newSquare) currentTurn)))   
        (begin (DrawBoard madeMove) (CheckTurn (* currentTurn -1) madeMove))
        (begin (InvalidMove) (CheckTurn currentTurn string))
        );end if and Lateral? Diagonal? PieceColor?
    );end define King
  
  ;revisa si las dos casillas seleccionadas son la misma. Si lo son, pedirá volver a tocar
  ;casillas hasta que sea un movimiento válido. Si no lo son, empezará a ver qué pieza queremos mover.
  (if (equal? oldSquare newSquare)
      (begin (InvalidMove) (CheckTurn currentTurn string))
      ;revisa si la ficha es un peon blanco
      (if (equal? (string-ref string oldSquare) #\P)
          (WhitePawn)
          ;revisa si la ficha es un peon negro
          (if (equal? (string-ref string oldSquare) #\p)
              (BlackPawn)
              ;revisa si la ficha es un alfil
              (if (or (equal? (string-ref string oldSquare) #\a) (equal? (string-ref string oldSquare) #\A))
                  (Bishop)
                  ;revisa si la ficha es un caballo
                  (if (or (equal? (string-ref string oldSquare) #\c) (equal? (string-ref string oldSquare) #\C))
                      (Knight)
                      ;revisa si la ficha es una torre
                      (if (or (equal? (string-ref string oldSquare) #\t) (equal? (string-ref string oldSquare) #\T))
                          (Tower)
                          ;revisa si la ficha es una dama
                          (if (or (equal? (string-ref string oldSquare) #\d) (equal? (string-ref string oldSquare) #\D))
                              (Queen)
                              ;revisa si la ficha es un rey
                              (if (or (equal? (string-ref string oldSquare) #\r) (equal? (string-ref string oldSquare) #\R))
                                  (King)
                                  ;si ninguna se cumple (lo que significa que presionó una casilla vacía) repite el turno
                                  (CheckTurn currentTurn string)
                                  );end if... (King)
                              );end if... (Queen)
                          );end if... (Tower)
                      );end if... (Knight)
                  );end if... (Bishop)
              );end if... (BlackPawn)
          );end if... (WhitePawn)
      );end if equal? (square column row) (square newPos-x newPos-y)
  );end define MovePiece

;funcion para el turno de las fichas blancas
(define (WhiteTurn string)
  ((draw-solid-rectangle chess) (make-posn 440 130) 120 40 "white")
  ((draw-string chess) (make-posn 465 155) "BLANCAS" "black")
  ;definimos la posicion de la pieza que queremos mover y sus correspondientes casillas
  (define pieceClick1 (mouse-click-posn (get-mouse-click chess)))
  ((draw-solid-rectangle chess) (make-posn 440 180) 120 40 "brown")
  (define piecePosx (position-x pieceClick1))
  (define piecePosy (position-y pieceClick1))
  (if (and (< (posn-x pieceClick1) 560) (> (posn-x pieceClick1) 440) (< (posn-y pieceClick1) 370) (> (posn-y pieceClick1) 350))
      (close-viewport chess)
  ;si la posicion clickeada se sale del tablero o su pieza no es del color correcto o está vacia, pedirá clickear de nuevo
  (if (or (>= piecePosx 8) (equal? (PieceColor? string (square piecePosx piecePosy)) 0))
      (WhiteTurn string)
      (if
       (equal? (PieceColor? string (square piecePosx piecePosy)) -1)
       (begin
         ((draw-solid-rectangle chess) (make-posn 440 180) 120 40 "black")
         ((draw-string chess) (make-posn 480 195) "No es" "white")
         ((draw-string chess) (make-posn 475 215) "tu turno!" "white")
         (WhiteTurn string))
       ;si la posicion tiene una pieza correcta, llamará a MovePiece con otro click más, esta vez para la casilla destino
       (MovePiece string piecePosx piecePosy (mouse-click-posn (get-mouse-click chess)) 1)
       );end if not equal? PieceColor? 1
      );end if >= piecePosx 8
    );end if and posn-x...
  );end define WhiteTurn

;funcion para el turno de las fichas negras
(define (BlackTurn string)
  ((draw-solid-rectangle chess) (make-posn 440 130) 120 40 "black")
  ((draw-string chess) (make-posn 470 155) "NEGRAS" "white")
  ;definimos  como pieceClick1 la posicion de la pieza que queremos mover y sus correspondientes casillas
  (define pieceClick1 (mouse-click-posn (get-mouse-click chess)))
  ((draw-solid-rectangle chess) (make-posn 440 180) 120 40 "brown")
  (define piecePosx (position-x pieceClick1))
  (define piecePosy (position-y pieceClick1))
  (if (and (< (posn-x pieceClick1) 560) (> (posn-x pieceClick1) 440) (< (posn-y pieceClick1) 370) (> (posn-y pieceClick1) 350))
      (close-viewport chess)
  ;si la posicion clickeada se sale del tablero o su pieza no es del color correcto o está vacia, pedirá clickear de nuevo
  (if (or (>= piecePosx 8) (equal? (PieceColor? string (square piecePosx piecePosy)) 0))
      (BlackTurn string) 
      (if
       (equal? (PieceColor? string (square piecePosx piecePosy)) 1)
       (begin
         ((draw-solid-rectangle chess) (make-posn 440 180) 120 40 "white")
         ((draw-string chess) (make-posn 480 195) "No es" "black")
         ((draw-string chess) (make-posn 475 215) "tu turno!" "black")
         (BlackTurn string))
       ;si la posicion tiene una pieza correcta, llamará a MovePiece con otro click más, esta vez para la casilla destino
       (MovePiece string piecePosx piecePosy (mouse-click-posn (get-mouse-click chess)) -1)
       );end if not equal? PieceColor? -1
      );end if >= piecePosx 8
  );end if and posn-x...
  );end define BlackTurn  

;definimos las posiciones iniciales de las fichas en una string
(define beginStr (string-append
                  "tcadract"
                  "pppppppp"
                  "zzzzzzzz"
                  "zzzzzzzz"
                  "zzzzzzzz"
                  "zzzzzzzz"
                  "PPPPPPPP"
                  "TCADRACT" );end string-append
  ) ;end define beginStr

;Empezamos la partida
(DrawBoard beginStr)
(WhiteTurn beginStr)
