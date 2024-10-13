#lang racket

;Aqui solamente creamos la interfaz ajena al tablero
(require graphics/graphics)
(open-graphics)
(define chess (open-viewport "Ajedrez" 600 400))
((draw-viewport chess) "brown")
((draw-string chess) (make-posn 470 70) "AJEDREZ" "snow")

;aqui definimos los nombres de las fichas por comodidad
(define torreBlanco "Torre blanco.png")
(define caballoBlanco "Caballo blanco.png")
(define alfilBlanco "Alfil blanco.png")
(define reyBlanco "Rey blanco.png")
(define reinaBlanco "Reina blanco.png")
(define peonBlanco "Peon blanco.png")
(define torreNegro "Torre negro.png")
(define caballoNegro "Caballo negro.png")
(define alfilNegro "Alfil negro.png")
(define reyNegro "Rey negro.png")
(define reinaNegro "Reina negro.png")
(define peonNegro "Peon negro.png")

;asociamos una imagen a cada caracter por usar
(define (EvaluateImage char)
  (if (equal? char #\T)
      torreBlanco
     (begin
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
      );end begin
      );end if char T
  );end define EvaluateImage

;La funcion PieceForDisplay pondrá todas las piezas en orden dependiendo de la string dada
(define (PieceForDisplay string counter)
  (if
   (< counter (string-length string))
   ;si recibe el caracter z no pondrá ninguna imagen
      (if (equal? (string-ref string counter) #\z)
          (PieceForDisplay string (+ counter 1))
          ;si recibe un caracter distinto a z, evaluará la imagen a colocar y la pondrá en su respectiva posición
                (begin (((draw-pixmap-posn (EvaluateImage (string-ref string counter))) chess) (make-posn (* (remainder counter 8) 50) (* 50 (quotient counter 8))))
                 (PieceForDisplay string (+ counter 1)));end begin
                );end if equal? string-ref #\z
      ;else,
      (void));end if < counter string-length
  );end define PieceForDisplay
;La funcion DrawBoard pondrá una tabla por encima de las piezas del turno anterior
(define (DrawBoard string)
((draw-pixmap chess) "Tablero 8x8.png" (make-posn 0 0))
  (PieceForDisplay string 0))
  
;CheckTurn va a llamar al siguiente turno. si recibe playerTurn=0, el siguiente turno será de las blancas; de lo contrario,
;si recibe playerTurn=1, el siguiente turno será de las negras. 
(define (CheckTurn playerTurn string)
  (if (equal? playerTurn 1)
      (WhiteTurn string)
      (BlackTurn string));end if playerTurn=0
  );end define CheckTurn

;la funcion PieceColor? revisará si la pieza es de color blanco, negro o si no hay ninguna pieza.
(define (PieceColor? string index)
  ;si la pieza es blanca devolverá 1
  (if (or (equal? (string-ref string index) #\P) (equal? (string-ref string index) #\T) (equal? (string-ref string index) #\C)
          (equal? (string-ref string index) #\A) (equal? (string-ref string index) #\R) (equal? (string-ref string index) #\D))
      1
  ;si la pieza es negra devolverá -1
  (if (or (equal? (string-ref string index) #\p) (equal? (string-ref string index) #\t) (equal? (string-ref string index) #\c)
          (equal? (string-ref string index) #\a) (equal? (string-ref string index) #\r) (equal? (string-ref string index) #\d))
      -1
      ;si no es ninguna, devolverá 0
      0)))

;servirá para crear una string nueva la cual indicará las posiciones del ajedrez
(define (MakeNewString char oldString prevPos nextPos)
  (define newString (string char))
  ;si recibe dos posiciones iguales, no hace nada, para evitar detener el flujo del programa
  (if (= nextPos prevPos)
  (void)
  ;else,
  (if (> nextPos prevPos)
  (string-append (substring oldString 0 prevPos) "z" (substring oldString (+ prevPos 1) nextPos) (substring newString 0 1) (substring oldString (+ nextPos 1)))
  (string-append (substring oldString 0 nextPos) (substring newString 0 1) (substring oldString (+ nextPos 1) prevPos) "z" (substring oldString (+ prevPos 1)))
  );end if > nextPos prevPos
 );end if = nextPos prevPos
);end define MakeNewString

;funciones para la comodidad de encontrar casillas
(define (pos-x click) (quotient (posn-x click) 50))
(define (pos-y click) (quotient (posn-y click) 50))
(define (square a b) (+ a (* b 8)))

;esta es la funcion que genera el movimiento, y engloba a todas las funciones de movimiento de cada pieza
(define (MovePiece string column row newPos currentTurn)
  (define newPos-x (pos-x newPos))
  (define newPos-y (pos-y newPos))
    (define oldSquare (square column row))
  (define newSquare (square newPos-x newPos-y))
  (define madeMove (MakeNewString (string-ref string oldSquare) string oldSquare newSquare))

  ;revisa si una casilla está diagonal a otra
  (define (Diagonal? pos)
    (if (equal? pos 0)
        0
    (if (or
         (equal? oldSquare (- newSquare (* pos 9)));abajo a la derecha
         (equal? oldSquare (- newSquare (* pos 7)));abajo a la izquierda
         (equal? oldSquare (+ newSquare (* pos 7)));arriba a la derecha
         (equal? oldSquare (+ newSquare (* pos 9)));arriba a la izquierda
        ) ;end or
        1
        ;else,
        (Diagonal? (- pos 1))
    );end if or
  );end if equal pos 0
);end define Diagonal?

  (define (Lateral? pos)
    (if (equal? pos 0)
        0
    (if (or
         (equal? oldSquare (- newSquare (* pos 8)));arriba
         (equal? oldSquare (+ newSquare (* pos 8)));abajo
         (equal? oldSquare (- newSquare pos));derecha
         (equal? oldSquare (+ newSquare pos));izquierda
        ) ;end or
        1
        ;else,
        (Lateral? (- pos 1))
    );end if or
  );end if equal pos 0
);end define Lateral?
  
  ;Movimiento de los peones blancos. Se separa de los peones negros debido a la dirección de avance de cada uno de ellos.
  (define (WhitePawn)
    ;revisa si está en la fila 7 del tablero (la cual es nombrada fila 6 en la string), para permitir el doble avance del peón
        (if (and (= row 6) (= oldSquare (+ newSquare 16)) (equal? (string-ref string newSquare) #\z))
        (begin (DrawBoard madeMove) (CheckTurn -1 madeMove))
        (begin
    ;si no está en la fila 7, revisa si la casilla seleccionada está justo adelante del peón, para permitir su avance
        (if (and (= oldSquare (+ newSquare 8)) (equal? (string-ref string newSquare) #\z))
        (begin (DrawBoard madeMove) (CheckTurn -1 madeMove))
         (if (and ;si no se cumple, revisa si está en diagonal al peón y hay otra pieza allí, para permitir capturarla
              (or  (= oldSquare (+ newSquare 7))
                   (= oldSquare (+ newSquare 9)))
             (not (equal? (string-ref string newSquare) #\z))
             (= (PieceColor? string newSquare) -1))
            (begin (DrawBoard madeMove) (CheckTurn -1 madeMove))
            ;si ninguno de estos casos se cumple, se reiniciará el turno con CheckTurn
        (CheckTurn currentTurn string))))))
  ;Movimiento de los peones negros. Se separa de los peones blancos debido a la dirección de avance de cada uno de ellos.
  (define (BlackPawn)
    ;revisa si está en la fila 2 del tablero (la cual es nombrada fila 1 en la string), para permitir el doble avance del peón
        (if (and (= row 1) (= oldSquare (- newSquare 16)) (equal? (string-ref string newSquare) #\z)) 
        (begin (DrawBoard madeMove) (CheckTurn 1 madeMove))
        (begin
    ;si no está en la fila 2, revisa si la casilla seleccionada está justo adelante del peón, para permitir su avance
        (if (and (= oldSquare (- newSquare 8)) (equal? (string-ref string newSquare) #\z))
        (begin (DrawBoard madeMove) (CheckTurn 1 madeMove))
        (if (and ;si no se cumple, revisa si está en diagonal al peón y hay otra pieza allí, para permitir capturarla
              (or (= oldSquare (- newSquare 7))
                  (= oldSquare (- newSquare 9)))
             (not (equal? (string-ref string newSquare) #\z))
             (= (PieceColor? string newSquare) 1))
            (begin (DrawBoard madeMove) (CheckTurn 1 madeMove))
    ;si ninguno de estos casos se cumple, se reiniciará el turno con CheckTurn
        (CheckTurn currentTurn string))))))
 ;Movimiento de los alfiles
  (define (Bishop)
    ;revisa si la casilla está en diagonal y no tiene una pieca del mismo color. Si no, se reiniciará el turno con CheckTurn
    (if (and (= (Diagonal? 7) 1) (not (equal? (PieceColor? string newSquare) currentTurn)))
        (begin (DrawBoard madeMove) (CheckTurn (* currentTurn -1) madeMove))
        (CheckTurn currentTurn string)
        );end if Diagonal?
  );end define Bishop
 ;Movimiento de las torres
  (define (Tower)
    ;revisa si la casilla está hacia alguno de los lados y no tiene una pieza del mismo color. Si no, se reiniciará el turno con CheckTurn
    (if (and (= (Lateral? 7) 1) (not (equal? (PieceColor? string newSquare) currentTurn)))
        (begin (DrawBoard madeMove) (CheckTurn (* currentTurn -1) madeMove))
        (CheckTurn currentTurn string));end if Lateral?
  );end define Tower
  ;Movimiento de la reina
  (define (Queen)
    ;revisa si la casilla está hacia los lados o en diagonal y no tiene una pieza del mismo color. Si no, se reiniciará el turno con CheckTurn
    (if (and (or (= (Lateral? 7) 1) (= (Diagonal? 7) 1)) (not (equal? (PieceColor? string newSquare) currentTurn)))
        (begin (DrawBoard madeMove) (CheckTurn (* currentTurn -1) madeMove))
        (CheckTurn currentTurn string)
        );end if and Lateral? Diagonal? PieceColor?
 );end define Queen
  ;Movimiento del rey
  (define (King)
    ;revisa si la casilla está hacia los lados o en diagonal a una sola casilla de distancia y no tiene una pieza del mismo color. Si no, se reiniciará el turno con CheckTurn
    (if (and (or (= (Lateral? 1) 1) (= (Diagonal? 1) 1)) (not (equal? (PieceColor? string newSquare) currentTurn)))
        (begin (DrawBoard madeMove) (CheckTurn (* currentTurn -1) madeMove))
        (CheckTurn currentTurn string)
        );end if and Lateral? Diagonal? PieceColor?
 );end define King
  ;revisa si las dos casillas seleccionadas son la misma. Si lo son, pedirá volver a tocar
  ;casillas hasta que sea un movimiento válido. Si no lo son, empezará a ver qué pieza queremos mover.
  (if (equal? oldSquare newSquare)
      (CheckTurn currentTurn string)
    ;revisa si la ficha es un peon blanco
    (if (equal? (string-ref string oldSquare) #\P)
        (WhitePawn)
    ;revisa si la ficha es un peon negro
        (if (equal? (string-ref string oldSquare) #\p)
        (BlackPawn)
    ;revisa si la ficha es un alfil
        (if (or (equal? (string-ref string oldSquare) #\a) (equal? (string-ref string oldSquare) #\A))
        (Bishop)
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
        );end if... (Bishop)
        );end if... (BlackPawn)
        );end if... (WhitePawn)
  );end if equal? (square column row) (square newPos-x newPos-y)
);end define MovePiece

;funcion para el movimiento de las blancas
(define (WhiteTurn string)
  ;definimos la posicion de la pieza que queremos mover y sus correspondientes casillas
  (define pieceClick1 (mouse-click-posn (get-mouse-click chess)))
  (define piecePosx (pos-x pieceClick1))
  (define piecePosy (pos-y pieceClick1))
  ;si la posicion clickeada se sale del tablero o su pieza no es del color correcto o está vacia, pedirá clickear de nuevo
  (if (or (>= piecePosx 8) (not (equal? (PieceColor? string (square piecePosx piecePosy)) 1)))
        (WhiteTurn string)
  ;si la posicion tiene una pieza correcta, llamará a MovePiece con otro click más, esta vez para la casilla destino
          (MovePiece string piecePosx piecePosy (mouse-click-posn (get-mouse-click chess)) 1))
  );end define WhiteTurn

;funcion para el movimiento de las blancas
(define (BlackTurn string)
   ;definimos la posicion de la pieza que queremos mover y sus correspondientes casillas
  (define pieceClick1 (mouse-click-posn (get-mouse-click chess)))
  (define piecePosx (pos-x pieceClick1))
  (define piecePosy (pos-y pieceClick1))
   ;si la posicion clickeada se sale del tablero o su pieza no es del color correcto o está vacia, pedirá clickear de nuevo
  (if (or (>= piecePosx 8) (not (equal? (PieceColor? string (square piecePosx piecePosy)) -1)))
        (BlackTurn string)
   ;si la posicion tiene una pieza correcta, llamará a MovePiece con otro click más, esta vez para la casilla destino
          (MovePiece string piecePosx piecePosy (mouse-click-posn (get-mouse-click chess)) -1))
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
"TCADRACT"
))

;Empezamos la partida
(DrawBoard beginStr)
(WhiteTurn beginStr)
