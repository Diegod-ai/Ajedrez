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
      );end if playerTurn=1
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
  (define charStr (string char))
  ;si recibe dos posiciones iguales, no hace nada, para evitar detener el flujo del programa
  (if (or (= nextPos prevPos) (> nextPos 63) (> prevPos 63))
      oldString
      ;else,
      (if (> nextPos prevPos)
          (string-append (substring oldString 0 prevPos) "z" (substring oldString (+ prevPos 1) nextPos) (substring charStr 0 1) (substring oldString (+ nextPos 1)))
          (string-append (substring oldString 0 nextPos) (substring charStr 0 1) (substring oldString (+ nextPos 1) prevPos) "z" (substring oldString (+ prevPos 1)))
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
    (string-append (substring str 0 square) (~a char) (substring str (+ square 1)))
    );end CoronationString
  ;ChoosePiece le pide un click al usuario y dependiendo del color del peon a coronar,
  ;indicado por el parametro color, crea en el tablero la nueva posición
  (define (ChoosePiece color)
    ;definimos click como la posicion en horizontal de 0 a 3 entre las 4 piezas del click que da el usuario,
    ; y la usamos luego para escoger con cual pieza reemplazaremos el peon
    (define click (position-x (mouse-click-posn (get-mouse-click coronationWindow))))
    (define coronationStringWhite (CoronationString strBoard position (string-ref "ACTD" click)))    
    (define coronationStringBlack (CoronationString strBoard position (string-ref "actd" click)))
    ;si el color es 1, usará las piezas con caracteres de letra mayuscula (los cuales representan piezas blancas)
    (if (= color 1)
        (begin (close-viewport coronationWindow) ((draw-solid-rectangle chess) (make-posn 440 180) 120 40 "brown")
               (DrawBoard coronationStringWhite)
               (if (GiveCheck? coronationStringWhite (* currentTurn -1))
                   (if (Mate? coronationStringWhite 0 0)
                       (begin (MateScreen) (FinishGame));end begin
                       (MoveOnCheck));end if Mate?
                   (void));end if GiveCheck?
               (CheckTurn (* pieceColor -1) (CoronationString strBoard position (string-ref "ACTD" click)))
               );end begin
        ;si el color no es 1, usará las piezas con caracteres de letra minuscula (los cuales representan piezas negras)
        (begin (close-viewport coronationWindow) ((draw-solid-rectangle chess) (make-posn 440 180) 120 40 "brown")
               (DrawBoard coronationStringBlack)              
               (if (GiveCheck? coronationStringBlack (* currentTurn -1))
                   (if (Mate? coronationStringBlack 0 0)
                       (begin (MateScreen) (FinishGame));end begin
                       (MoveOnCheck));end if Mate?
                   (void));end if GiveCheck
               (CheckTurn (* pieceColor -1) coronationStringBlack)
               );end begin
        );end if = color 1
    );end define ChoosePiece
  ;revisa si el color de la pieza que le dimos es blanco. Si lo es, dibuja un recuadro de color negro para que haga de fondo a las piezas
  ;si no es blanco, simplemente dibujará las piezas, sin ningun tipo de fondo detrás de ellas
  (if (= pieceColor 1)
      (begin ((draw-solid-rectangle coronationWindow) (make-posn 0 0) 200 50 "black")
             (PieceForDisplay "ACTD" 0 coronationWindow));end begin
      (PieceForDisplay "actd" 0 coronationWindow)
      );end if = pieceColor 1
  (ChoosePiece pieceColor)
  );end define Coronation

  ;revisa si una casilla está diagonal a otra, si lo está devuelve la distancia en la string entre las casillas
  ;pos es un contador, a es la casilla inicial y b es la casilla destino
  (define (Diagonal? pos a b)
    (define columnPiece (remainder a 8))
    (define columnNewSquare (remainder b 8))
        ;si el contador pos llega a cero, devolvemos 0, lo cual significa que la casilla no está diagonal a la otra
        (if (equal? pos 0) 0
            ;si alguno de estos casos se cumple, devuelve pos*9 o pos*7 dependiendo de la dirección
            (if (and (equal? b (+ a (* pos 9)))  (not (>= columnPiece columnNewSquare))) (* pos 9)  ;devuelve pos*9
            (if (and (equal? b (+ a (* pos 7)))  (not (<= columnPiece columnNewSquare))) (* pos 7)  ;devuelve pos*7
            (if (and (equal? b (+ a (* pos -9))) (not (<= columnPiece columnNewSquare))) (* pos -9) ;devuelve pos*-9
            (if (and (equal? b (+ a (* pos -7))) (not (>= columnPiece columnNewSquare))) (* pos -7) ;devuelve pos*-7
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
            (if (and (equal? b (+ a (* pos 8))) (= (remainder a 8) (remainder b 8))) (* pos 8) ;indica que se mueve en vertical
            (if (and (equal? b (+ a pos))       (= (quotient a 8) (quotient b 8)))       pos    ;indica que se mueve en horizontal
            (if (and (equal? a (+ b (* pos 8))) (= (remainder b 8) (remainder a 8))) (* pos -8);indica que se mueve en vertical
            (if (and (equal? a (+ b pos))       (= (quotient b 8) (quotient a 8)))    (- pos)   ;indica que se mueve en horizontal
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
    (define x-distance (- (quotient a 8) (quotient b 8)))
    (define y-distance (- (remainder a 8) (remainder b 8)))
        ;si a es menor que b, se invierten los valores
    (if (< b a) (LShape? b a)
        ;revisa si una casilla está en direccion de L a otra
        (if (or (and (= x-distance 1) (= y-distance 2))
                (and (= x-distance 1) (= y-distance -2))
                (and (= x-distance -1) (= y-distance 2))
                (and (= x-distance -1) (= y-distance -2))
                (and (= x-distance 2) (= y-distance 1))
                (and (= x-distance 2) (= y-distance -1))
                (and (= x-distance -2) (= y-distance 1))
                (and (= x-distance -2) (= y-distance -1)));end or
            1 ;devuelve 1, lo que significa que se cumple
            -1 ;si no, devuelve -1, lo que significa que no se cumple
            );end if or
        );end if < a b
    );end define LShape?

  ;Revisa si una casilla está en jaque
  (define (OnCheck? str square kingColor)
    ;revisa si una casilla está en un jaque de caballo
    (define (KnightCheck? value)
      (if (or (> (+ value square) 63) (< (+ value square) 0) (= (LShape? square (+ square value)) -1))
          #f
      (if (= kingColor 1)
          (equal? (string-ref str (+ square value)) #\c)
          (equal? (string-ref str (+ square value)) #\C)
      );end if = kingColor 1
      );end if > (+ value square) 63
     );end define KnightCheck?
    (define (DiagonalCheck? distance direction)
      (define piece (+ square (* distance direction)))
      (if (= distance 8)
                   #f
          (if (= kingColor 1)
              (if (or (> piece 63)
                      (< piece 0))
                  #f
            (if (and
                 (or
                  (and (or (= direction -7) (= direction 9)) (> (remainder piece 8) (remainder square 8)))
                  (and (or (= direction 7) (= direction -9)) (< (remainder piece 8) (remainder square 8))))
                 (or
                     (equal? (string-ref str piece) #\a)
                     (equal? (string-ref str piece) #\d)
                     (and (= distance 1) (equal? (string-ref str piece) #\r)))
                 (equal? (Restrictions str (- (* distance direction) direction) direction square piece) 0));end and
                #t
            (DiagonalCheck? (+ distance 1) direction)
            );end if equal? string-ref
            );end if or >...
          ;else (= kingColor -1),
              (if (or (> piece 63) (< piece 0))
                  #f
            (if (and
                 (or
                  (and (or (= direction -7) (= direction 9)) (> (remainder piece 8) (remainder square 8)))
                  (and (or (= direction 7) (= direction -9)) (< (remainder piece 8) (remainder square 8))))                 
                 (or (equal? (string-ref str piece) #\A)
                     (equal? (string-ref str piece) #\D)
                     (and (= distance 1) (equal? (string-ref str piece) #\R)))
                 (equal? (Restrictions str (- (* distance direction) direction) direction square piece) 0))
                #t
            (DiagonalCheck? (+ distance 1) direction)
            );end if equal? string-ref
            );end if or >...
          );end if = kingColor 1
          );end if = distance 8
      );end define DiagonalCheck?
    (define (LateralCheck? distance direction)
      (define piece (+ square (* distance direction)))
      (if (= distance 8)
              #f
          (if (= kingColor 1)
              (if (or
                   (> piece 63)
                   (< piece 0))
                  #f
          (if (and (or
                    (equal? (quotient piece 8) (quotient square 8))
                    (equal? (remainder piece 8) (remainder square 8)))
                   (or (equal? (string-ref str piece) #\t)
                       (equal? (string-ref str piece) #\d)
                       (and (= distance 1) (equal? (string-ref str piece) #\r)))
                 (equal? (Restrictions str (- (* direction distance) direction) direction square piece) 0))
                #t
            (LateralCheck? (+ distance 1) direction)
            );end if equal? string-ref
            );end if or >...
          ;else (= kingColor -1),
              (if (or (> piece 63) (< piece 0))
                  #f
            (if (and (or
                      (equal? (quotient piece 8) (quotient square 8))
                      (equal? (remainder piece 8) (remainder square 8)))
                 (or (equal? (string-ref str (+ (* distance direction) square)) #\T)
                     (equal? (string-ref str (+ (* distance direction) square)) #\D)
                     (and (= distance 1) (equal? (string-ref str piece) #\R)))
                 (equal? (Restrictions str (- (* direction distance) direction) direction square piece) 0))
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
               (and (equal? (string-ref str (- square 7)) #\p) (> (remainder (- square 7) 8) (remainder square 8)))
               (and (equal? (string-ref str (- square 9)) #\p) (< (remainder (- square 9) 8) (remainder square 8)))
               ) #t #f);end if or equal? string-ref...
          );end if or <...
          (if (or (> (+ square 7) 63) (> (+ square 9) 63)) #f
          (if (or
              (and (equal? (string-ref str (+ square 7)) #\P) (< (remainder (+ square 7) 8) (remainder square 8)))
              (and (equal? (string-ref str (+ square 9)) #\P) (> (remainder (+ square 9) 8) (remainder square 8)))
               ) #t #f);end if or equal? string-ref...
          );end if or <...
          );end if = kingColor 1
      );end define PawnCheck?
    (if (or
         (KnightCheck? -6)
         (KnightCheck? -10)
         (KnightCheck? -15)
         (KnightCheck? -17)
         (KnightCheck? 6)
         (KnightCheck? 10)
        (KnightCheck? 15)
         (KnightCheck? 17)
         (DiagonalCheck? 1 -9)
         (DiagonalCheck? 1 -7)
         (DiagonalCheck? 1 7)
         (DiagonalCheck? 1 9)       
         (LateralCheck? 1 -8)       
         (LateralCheck? 1 -1)       
         (LateralCheck? 1 1)       
         (LateralCheck? 1 8)
         (PawnCheck?)
         );end or
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
    (if (= pos 64) pos
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

(define (Movements str oldSq newSq rowPiece columnPiece turn)
  ;Movimiento de los peones blancos. Se separa de los peones negros debido a la dirección de avance de cada uno de ellos.
  (define (WhitePawn)
    ;revisa si está en la fila 7 del tablero (la cual es nombrada fila 6 en la string), para permitir el doble avance del peón
    (if (and (= rowPiece 6) (= oldSq (+ newSq 16)) (equal? (string-ref str newSq) #\z))
        #t
        (begin
          ;si no está en la fila 7, revisa si la casilla seleccionada está justo adelante del peón, para permitir su avance
          (if (and (= oldSq (+ newSq 8)) (equal? (string-ref str newSq) #\z))
              ;aqui revisa esi el movimiento dejo al peon en una casilla de coronacion
              (if (= newPos-y 0)
                  1
                  #t
                  );end if = * 8 newPos-y 0
              ;si no se cumple, revisa si está en diagonal al peón y hay otra pieza allí, para permitir capturarla
              (if (and (not (equal? (string-ref str newSq) #\z))
                       (= (PieceColor? string newSq) -1)
                       (or  (and (= oldSq (+ newSq 7)) (> newPos-x columnPiece))
                            (and (= oldSq (+ newSq 9)) (< newPos-x columnPiece)));end or
                       ) ;end and
                  ;aqui revisa si la captura dejó al peón en una casilla de coronación
                  (if (= newPos-y 0)
                      1
                      #t
                      );end if = * 8 newPos-y 0
                  ;si ninguno de estos casos se cumple, se reiniciará el turno con CheckTurn
                  #f
                  );end if and or (captura diagonal)
              );end if (avance de una casilla)
          );end begin
        );end if (avance de doble casilla)
    );end define WhitePawn
  ;Movimiento de los peones negros. Se separa de los peones blancos debido a la dirección de avance de cada uno de ellos.
  (define (BlackPawn)
    ;revisa si está en la fila 2 del tablero (la cual es nombrada fila 1 en la string), para permitir el doble avance del peón
    (if (and (= rowPiece 1) (= oldSq (- newSq 16)) (equal? (string-ref str newSq) #\z)) 
        #t
        (begin
          ;si no está en la fila 2, revisa si la casilla seleccionada está justo adelante del peón, para permitir su avance
          (if (and (= oldSq (- newSq 8)) (equal? (string-ref str newSq) #\z))
              ;aqui revisa si el movimiento dejó al peón en una casilla de coronación
              (if (= newPos-y 7)
                  -1
                  #t
                  );end if = * 8 newPos-y 0
              (if (and ;si no se cumple, revisa si está en diagonal al peón y hay otra pieza allí, para permitir capturarla
                   (or (and (= oldSq (- newSq 7)) (< newPos-x columnPiece))
                       (and (= oldSq (- newSq 9)) (> newPos-x columnPiece)))
                   (not (equal? (string-ref str newSq) #\z))
                   (= (PieceColor? str newSq) 1))
                  ;aqui revisa si la captura dejó al peón en una casilla de coronación
                  (if (= newPos-y 7)
                      -1
                      #t
                      );end if = * 8 newPos-y 0
                  ;si ninguno de estos casos se cumple, se reiniciará el turno con CheckTurn
                  #f)))))
  ;Movimiento de los alfiles
  (define (Bishop)
    ;revisa si la casilla está en diagonal y no tiene una pieca del mismo color. Si no, se reiniciará el turno con CheckTurn    
    (define distance (Diagonal? 7 oldSq newSq))
    (define direction (DiagonalDirection? 7 oldSq newSq))
    (if (and (not (= distance 0)) (equal? (Restrictions str (- distance direction) direction oldSq newSq) 0)
             (not (equal? (PieceColor? str newSq) turn)))
        #t
        ;else,
        #f
        );end if Diagonal?
    );end define Bishop
  (define (Knight)
    ;revisa si la casilla está en diagonal y no tiene una pieca del mismo color. Si no, se reiniciará el turno con CheckTurn
    (if (and (= (LShape? oldSq newSq) 1) (not (equal? (PieceColor? str newSq) turn)))
        #t
        #f
        );end if LShape?
    );end define Knight
  ;Movimiento de las torres
  (define (Tower)
    (define distance (Lateral? 7 oldSq newSq))
    (define direction (LateralDirection? 7 oldSq newSq))
    ;revisa si la casilla está hacia alguno de los lados y no tiene una pieza del mismo color. Si no, se reiniciará el turno con CheckTurn
    (if (and (not (= distance 0)) (equal? (Restrictions str (- distance direction) direction oldSq newSq) 0)
             (not (equal? (PieceColor? str newSq) turn)))
        #t #f
        );end if
    );end define Tower
  ;Movimiento de la reina
  (define (Queen)
    (define lateralDistance (Lateral? 7 oldSq newSq))
    (define lateralDirection (LateralDirection? 7 oldSq newSq))
    (define diagonalDistance (Diagonal? 7 oldSq newSq))
    (define diagonalDirection (DiagonalDirection? 7 oldSq newSq))
    ;revisa si la casilla está hacia los lados o en diagonal y no tiene una pieza del mismo color. Si no, se reiniciará el turno con CheckTurn
    (if (and (or
              (and (not (= lateralDistance 0)) (equal? (Restrictions str (- lateralDistance lateralDirection) lateralDirection oldSq newSq) 0))
              (and (not (= diagonalDistance 0)) (equal? (Restrictions str (- diagonalDistance diagonalDirection) diagonalDirection oldSq newSq) 0))
              );end or
             (not (equal? (PieceColor? str newSq) turn)))
                  #t
                  #f
        );end if and Lateral? Diagonal? PieceColor?
    );end define Queen
  ;Movimiento del rey
  (define (King)
    ;revisa si la casilla está hacia los lados o en diagonal a una sola casilla de distancia y no tiene una pieza del mismo color. Si no, se reiniciará el turno con CheckTurn
    (if (and
         (or (not (= (Lateral? 1 oldSq newSq) 0)) (not (= (Diagonal? 1 oldSq newSq) 0)))
         (not (equal? (PieceColor? str newSq) turn)))   
        #t
        #f
        );end if and Lateral? Diagonal? PieceColor?
    );end define King
  
  ;revisa si las dos casillas seleccionadas son la misma. Si lo son, pedirá volver a tocar
  ;casillas hasta que sea un movimiento válido. Si no lo son, empezará a ver qué pieza queremos mover.
  (if (equal? oldSq newSq)
      #f
      ;revisa si la ficha es un peon blanco
      (if (equal? (string-ref str oldSq) #\P)
          (WhitePawn)
          ;revisa si la ficha es un peon negro
          (if (equal? (string-ref str oldSq) #\p)
              (BlackPawn)
              ;revisa si la ficha es un alfil
              (if (or (equal? (string-ref str oldSq) #\a) (equal? (string-ref str oldSq) #\A))
                  (Bishop)
                  ;revisa si la ficha es un caballo
                  (if (or (equal? (string-ref str oldSq) #\c) (equal? (string-ref str oldSq) #\C))
                      (Knight)
                      ;revisa si la ficha es una torre
                      (if (or (equal? (string-ref str oldSq) #\t) (equal? (string-ref str oldSq) #\T))
                          (Tower)
                          ;revisa si la ficha es una dama
                          (if (or (equal? (string-ref str oldSq) #\d) (equal? (string-ref str oldSq) #\D))
                              (Queen)
                              ;revisa si la ficha es un rey
                              (if (or (equal? (string-ref str oldSq) #\r) (equal? (string-ref str oldSq) #\R))
                                  (King)
                                  ;si ninguna se cumple (lo que significa que presionó una casilla vacía) repite el turno
                                  #f
                                  );end if... (King)
                              );end if... (Queen)
                          );end if... (Tower)
                      );end if... (Knight)
                  );end if... (Bishop)
              );end if... (BlackPawn)
          );end if... (WhitePawn)
      );end if equal? (square column row) (square newPos-x newPos-y)
    );end define Movements
  
  (define (Mate? mateStr piecePos pieceSquare)
    (define break? (MakeNewString (string-ref mateStr piecePos) mateStr piecePos pieceSquare))
    (if (GiveCheck? mateStr (* currentTurn -1))
            (if
             (or (= (PieceColor? mateStr piecePos) currentTurn) (equal? (string-ref mateStr piecePos) #\z))
                    (if (= piecePos 63) #t (Mate? mateStr (+ piecePos 1) 0))
                    (if (and
                         (not (GiveCheck? break? (* currentTurn -1)))
                         (equal? (Movements mateStr piecePos pieceSquare (quotient piecePos 8) (remainder piecePos 8) (* currentTurn -1)) #t))    
                     #f
                     (if (= pieceSquare 63)
                         (if (= piecePos 63) #t (Mate? mateStr (+ piecePos 1) 0)) (Mate? mateStr piecePos (+ pieceSquare 1)))) ;end if not GiveCheck?
                );end if or
        #f );end if GiveCheck?
    );end define Mate?
(define (MateScreen)
    (if (= currentTurn 1)
         (begin
           ((draw-solid-rectangle chess) (make-posn 100 132) 200 132 "white")
           ((draw-string chess) (make-posn 150 190) "JAQUE MATE" "brown")
           ((draw-string chess) (make-posn 153 210) "Ganan blancas!" "brown"))
        (begin ((draw-solid-rectangle chess) (make-posn 100 132) 200 132 "black")
               ((draw-string chess) (make-posn 150 190) "JAQUE MATE" "white")
               ((draw-string chess) (make-posn 153 210) "Ganan negras!" "white")))
        ((draw-rectangle chess) (make-posn 101 133) 198 130 "brown"))

(define (CallNextMove)
  (if (Mate? madeMove 0 0)
    (begin (DrawBoard madeMove) (MateScreen) (FinishGame))
    (if
    (number? (Movements string oldSquare newSquare row column currentTurn))
    (begin (DrawBoard madeMove) (Coronation madeMove newSquare currentTurn))
    (if (equal? (Movements string oldSquare newSquare row column currentTurn) #t)
    (begin (DrawBoard madeMove) (CheckTurn (* currentTurn -1) madeMove))
    (begin (InvalidMove) (CheckTurn currentTurn string))
    );if equal? Movements #t
    );end if Movements
    );end if Mate?
  );end define CallNextMove
  (CallNextMove)
  );end define MovePiece

(define (FinishGame)
  (define EndButton (mouse-click-posn (get-mouse-click chess)))
  (if (and (< (posn-x EndButton) 560) (> (posn-x EndButton) 440) (< (posn-y EndButton) 370) (> (posn-y EndButton) 350))
      (close-viewport chess)
      (FinishGame)))

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

(define debugStr (string-append
                  "zzzzzzzr"
                  "zppppppp"
                  "zzzzzzzz"
                  "Pzzzzzzz"
                  "zzzzzzzz"
                  "zzzzzzzz"
                  "zPPPPPPP"
                  "TCADRACT")
  )

;Empezamos la partida
(define (StartGame str)
(DrawBoard str)
(WhiteTurn str))

;(StartGame beginStr)
(StartGame debugStr)
