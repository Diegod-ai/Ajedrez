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
  (if (equal? playerTurn 0)
      (WhiteTurn string)
      (BlackTurn string));end if playerTurn=0
  );end define CheckTurn

;la funcion PieceColor? revisará si la pieza es de color blanco, negro o si no hay ninguna pieza.
(define (PieceColor? string index)
  ;si la pieza es blanca devolverá 0
  (if (or (equal? (string-ref string index) #\P) (equal? (string-ref string index) #\T) (equal? (string-ref string index) #\C)
          (equal? (string-ref string index) #\A) (equal? (string-ref string index) #\R) (equal? (string-ref string index) #\D))
      0
  ;si la pieza es negra devolverá 1
  (if (or (equal? (string-ref string index) #\p) (equal? (string-ref string index) #\t) (equal? (string-ref string index) #\c)
          (equal? (string-ref string index) #\a) (equal? (string-ref string index) #\r) (equal? (string-ref string index) #\d))
      1
      ;si no es ninguna, devolverá 2
      2)))

;servirá para crear una string nueva la cual indicará las posiciones del ajedrez
(define (MakeNewString char oldString prevPos nextPos)
  (define newString (string char))
  (if (= nextPos prevPos)
  (void)
  (if (> nextPos prevPos)
  (string-append (substring oldString 0 prevPos) "z" (substring oldString prevPos nextPos) (substring newString 0 1) (substring oldString (+ nextPos 1)))
  (string-append (substring oldString 0 nextPos) (substring newString 0 1) (substring oldString (+ nextPos 1) prevPos) "z" (substring oldString (+ prevPos 1)))
  )
 )
)

;funciones para la comodidad de encontrar casillas
(define (pos-x click) (quotient (posn-x click) 50))
(define (pos-y click) (quotient (posn-y click) 50))
(define (square a b) (+ a (* b 8)))

(define (MovePiece string column row newPos currentTurn)
  (define newPos-x (pos-x newPos))
  (define newPos-y (pos-y newPos))
  (define madeMove (MakeNewString (string-ref string (square column row)) string (square column row) (square newPos-x newPos-y)))
  (if (equal? (square column row) (square newPos-x newPos-y))
      (CheckTurn currentTurn string)
    (if (equal? (string-ref string (square column row)) #\P)
        (if (and (= row 6) (= (square column row) (+ (square newPos-x newPos-y) 16)) (equal? (string-ref string (square newPos-x newPos-y)) #\z)) 
        (begin (DrawBoard madeMove) (CheckTurn 0 madeMove))
        (begin
        (if (and (= (square column row) (+ (square newPos-x newPos-y) 8)) (equal? (string-ref string (square newPos-x newPos-y))  #\z))
        (begin (DrawBoard madeMove) (CheckTurn 0 madeMove))
        (if (and
             (or (= (square column row) (+ (square newPos-x newPos-y) 7)) (= (square column row) (+ (square newPos-x newPos-y) 9)))
             (not (equal? (string-ref string (square newPos-x newPos-y)) #\z)))
            (begin (DrawBoard madeMove) (CheckTurn 0 madeMove))
        (CheckTurn currentTurn string)))))
        (CheckTurn currentTurn string))
  )
)
    
(define (WhiteTurn string)
  (define pieceClick1 (mouse-click-posn (get-mouse-click chess)))
  (define piecePosx (pos-x pieceClick1))
  (define piecePosy (pos-y pieceClick1))
  (if (or (>= piecePosx 8) (not (equal? (PieceColor? string (square piecePosx piecePosy)) 0)))
        (WhiteTurn string)
          (MovePiece string piecePosx piecePosy (mouse-click-posn (get-mouse-click chess)) 0))
  );end define WhiteTurn

(define (BlackTurn string)
  (define pieceClick1 (mouse-click-posn (get-mouse-click chess)))
  (define piecePosx (quotient (posn-x pieceClick1) 50))
  (define piecePosy (quotient (posn-y pieceClick1) 50))  
  (if (>= piecePosx 8)
        (BlackTurn string)
     (if (equal? (PieceColor? string (+ piecePosx (* piecePosy 8))) 1)
        (begin
  (printf "~a ~a" piecePosx piecePosy)
  (((draw-pixmap-posn caballoNegro) chess) (make-posn (* 50 piecePosx) (* 50 piecePosy)))
 (WhiteTurn string))
        (begin
          (printf "Casilla no válida ")
          (BlackTurn string))))
  );end define BlackTurn

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
(DrawBoard beginStr)
(WhiteTurn beginStr)