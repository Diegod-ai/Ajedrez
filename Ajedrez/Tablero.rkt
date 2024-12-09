#lang racket

(require graphics/graphics )
(open-graphics) 
(define ventana1 (open-viewport "Ajedrez"  800 600))


(define (dibujar a b e)
  
  ((draw-string ventana1) (make-posn 200 30) "     AJEDREZ" "brown")
  (if (> e 8)
      ((draw-rectangle ventana1) (make-posn 50 50) 401 401 "black")
      (if (> a 400)
          (dibujar 50 (+ b 50) (+ e 1))
          (begin
            (if(odd? e)
               (if (odd? (/ a 50 ) )
                   (begin
                    ((draw-solid-rectangle ventana1)(make-posn a b) 50 50 "white")
                    (dibujar(+ a 50)b e))
                   (begin
                     ((draw-solid-rectangle ventana1)(make-posn a b) 50 50 "slate blue")
                     (dibujar(+ a 50)b e))     
                   )
               (if(odd?(/ a 50) )
                  (begin
                    ((draw-solid-rectangle ventana1)(make-posn a b) 50 50 "slate blue")
                    (dibujar(+ a 50)b e))
                  
                  (begin
                    ((draw-solid-rectangle ventana1)(make-posn a b) 50 50 "white")
                    (dibujar(+ a 50)b e))
                  )                  
               )
            )
          )
      )
  ) ;EndFunction Dibujar, sirve para guardar el tablero que se va a dibujar cuadro por cuadro 

; Este identificador guarda los parametros de dibujo y lo simplifica en un solo llamado que nombramos como "table"

(define (table tableString)
  (dibujar 50 50 1) (PiecePut tableString 0))

;fichas blancas
(define BishopW   "WhitePieces/whiteBishop.png")
(define KnightW "WhitePieces/whiteKnight.png")
(define rookW   "WhitePieces/whiteRook.png")
(define kingW     "WhitePieces/whiteKing.png")
(define queenW   "WhitePieces/whiteQueen.png")
(define pawnW    "WhitePieces/whitePawn.png" )


;fichas negras
(define BishopB   "BlackPieces/blackBishop.png")
(define KnightB "BlackPieces/blackKnight.png")
(define rookB   "BlackPieces/blackRook.png")
(define kingB     "BlackPieces/blackKing.png")
(define queenB   "BlackPieces/blackQueen.png")
(define pawnB "BlackPieces/blackPawn.png" )
 

;finalizar de llamar fichas negras


;PieceAsignament evalua el caracter que se encuentra en otra funcion y lo evalua con un caracter que
;se refiere a una pieza del ajedrez, y despues le afilia una imagen
( define ( PieceAsignament char)

   ( if ( equal? char #\T)
        rookB
        ( if ( equal? char #\C)
             KnightB
             ( if ( equal? char #\A)
                  BishopB
                  ( if ( equal? char #\D)
                       queenB
                       (if ( equal? char #\R )
                           kingB
                           (if ( equal? char #\P )
                               pawnB
                               ( if ( equal? char #\t)
                                    rookW
                                    ( if ( equal? char #\c)
                                         KnightW
                                         ( if ( equal? char #\a)
                                              BishopW
                                              ( if ( equal? char #\d)
                                                   queenW
                                                   (if ( equal? char #\r)
                                                       kingW
                                                       (if ( equal? char #\p )
                                                           pawnW 
                                                           "";else
                                                           ) ;end if ( equal? char #\p)
                                                       ) ;end if ( equal? char #\r)        
                                                   ) ;end if ( equal? char #\d)             
                                              ) ;end if ( equal? char #\a)                  
                                         ) ;end if ( equal? char #\c)                       
                                    ) ;end if ( equal? char #\t)
                               ) ;end if ( equal? char #\P) 
                           ) ;end if ( equal? char #\R)
                       ) ;end if ( equal? char #\D)
                  ) ;end if ( equal? char #\A)
             ) ;end if ( equal? char #\C)
        ) ;end if ( equal? char #\T)
   ) ;endFunction PieceAsignament

; InicialTable nos muestra el tablero inicial
(define inicialTable (string-append
                      "TCADRACT" ;fichas negras 
                      "pPPPPPPP" ;peones negros
                      "xxxxxxxx" ;espacio en blanco
                      "xxxxxxxx" ;espacio en blanco
                      "xxxxxxxx" ;espacio en blanco
                      "xxxxxxxx" ;espacio en blanco
                      "pPpppppp" ;peones blancos
                      "tcadract" ;fichas blancas
                      ))

;;Creamos la funcion PiecePut que sirve para poner la ficha en su lugar correspondiente
;;comparando cada parte de la funcion "InicialTable"
(define (PiecePut str count)
  
  (if (< count (string-length str))
      
      (if (not (equal? (string-ref str count) #\x))  
          (begin                                                                              
        
            (((draw-pixmap-posn (PieceAsignament (string-ref str count))) ventana1) (make-posn (* (+ 1 (remainder count 8)) 50) (* (+ 1 (quotient count 8)) 50)))
            (PiecePut str (+ count 1))
            );endBegin and start else
          (PiecePut str (+ count 1)) ;llama la funcion y le suma 1 al contador
               
          );end (if (not (equal? (string-ref str count) #\x)) and start else
      (void)        
      );end (if (< count (string-length str))
  ); EndFunction PiecePut
;en esta funcion, ponemos las fichas segun la comparacion que hizo arriba

;aqui llamamos la funcion que llama el tablero y las fichas
(table inicialTable )

;;------------------StartFunction Casillas-----------------;;

;;Aqui creamos una funcion global que facilitara los clicks en todo el codigo
;;funcion (casilla a b) cuando se llame tomara los diferentes
;;parametros que le asignaremos en las demas funciones

(define (casilla a b)
   ( - (+ a (* b 8)) 9 ))
(define (pos-x click) (quotient (posn-x click) 50))
(define (pos-y click) (quotient (posn-y click) 50))

;;------------------EndFunction Casillas-----------------;;

;;------------------StarFunction CasillasLimite-----------------;;

;;Esta funcion se llamara cuando necesitemos siempre que necesitemos un click
;;Pues esta funcion tiene como proposito evitar que se permitan los click's
;;Fuera del tablero de ajedrez
(define (limites posn)

  (define posnx (posn-x posn))
  (define posny (posn-y posn))
  
  (if (or (< posnx 50 ) (< posny 50 ) (> posnx 450 ) (> posny 450 ))
      (limites (mouse-click-posn (get-mouse-click ventana1)));else
      posn
      );endIf
  );;------------------EndFunction CasillasLimite-----------------;;


;;------------------StartFunction ColorComprobant-----------------;;

;;colorComprobant: Esta funcion tomara el click del usuario y la str que evaluara
;;segun el click que demos sabremos a que pieza nos referimos
;;Sirve para comprobar si la pieza que se oprime en el primer click es del turno al que correspode

(define (colorComprobant clickOn str)
  
  (if ( or (equal? (string-ref str clickOn) #\T ) (equal? (string-ref str clickOn) #\C )
           (equal? (string-ref str clickOn) #\A ) (equal? (string-ref str clickOn) #\D )
           (equal? (string-ref str clickOn) #\R ) (equal? (string-ref str clickOn) #\P ));EndCondition or
      ;Si la pieza es negra devolvera un 1
      1 ; else
    
      (if ( or (equal? (string-ref str clickOn) #\t ) (equal? (string-ref str clickOn) #\c )
               (equal? (string-ref str clickOn) #\a ) (equal? (string-ref str clickOn) #\d )
               (equal? (string-ref str clickOn) #\r ) (equal? (string-ref str clickOn) #\p ));EndCondition or
          ;si la pieza es blanca devolvera un -1 
          -1 ; else

          ; en caso contrario, no devuelve nada
          0 
          ); Endif que comprueba que las fichas son negras
      ); Endif que comprueba que las fichas son Blancas
  );;------------------EndFunction ColorComprobant-----------------;;

;;------------------StarFunction MakeString-----------------;;

;;MakeString: Esta funcion, estara actualizando las fichas del tablero según los movimientos que se hagan durante el juego
(define (MakeString oldString char oldPos newPos)
   ( if (= oldPos newPos)
       oldString
       (if (> oldPos newPos)
           (string-append (substring oldString 0 newPos) (~a char) (substring oldString (add1 newPos) oldPos) "x" (substring oldString (add1 oldPos)))
           (string-append (substring oldString 0 oldPos) "x" (substring oldString (add1 oldPos) newPos) (~a char) (substring oldString (add1 newPos)))
           );EndIf
       );endIf

  );;------------------EndFunction MakeString-----------------;;


;;------------------StarFunction restrictions-----------------;;
(define (restrictions str inicialPos endPos currentTurn)
  
  ;;Estos dos identificadores estan hechos para sacar la posicion en "X" y "Y"
  ;;de la posicion inicial de una pieza
  (define yPosInitial ( quotient inicialPos 8 ))
  (define xPosInitial (remainder inicialPos 8 ))
  
  ;;Estos dos identificadores estan hechos para sacar la posicion en "X" y "Y"
  ;;de la posicion final de una pieza
  (define yPosFinal ( quotient  endPos 8 ))
  (define xPosFinal (remainder endPos 8 ))
  
  ;;Este identificador saca la distancia que hay entre la posicion inicial y la posicion final
  (define distance  (- endPos inicialPos ) )

  ;;Este identificado, nos dira la direccion recta que tomara la ficha que seleccionemos
  
  
  (define directionLine
    ;;Esta condición revisara si el movimiento es hacia arriba
    (if (and (< yPosFinal yPosInitial) (= xPosFinal xPosInitial))
        -8
        ;;Esta condicion revisara si el movimiento es hacia abajo
        (if (and (> yPosFinal yPosInitial) (= xPosFinal xPosInitial))
            8
            ;;Esta condicion es para ver si el movimiento es hacia la izquierda
            (if (and (< xPosFinal xPosInitial) (= yPosFinal yPosInitial))
                -1
                ;;Esta condicion es para ver si el movimiento es hacia la derecha 
                (if (and (> xPosFinal xPosInitial) (= yPosFinal yPosInitial))
                    1
                    #f
                    )
                );EndIf
            );;EndIf
        );;EndIf

    )

  ;;Esta función revisara si en el trayecto de la pieza, está misma no salta alguna otra ficha
  ;;o se come alguna pieza de su misma clase
  ;;Adicionalmente comprobamos que no se coma al rey MUJEJEJE
  
  (define (nonJump stop)
    (displayln directionLine)
    (displayln stop)
    ;;Esta condicion la hacemos para determinar el turno en que estamos y el camino que tomara la comprobación
    ;;Empieza revisando si el turno es de las fichas negras
    (if  (equal? currentTurn 1 )
         
         ;;Esta condicion revisara si la posicion final de la ficha es una ficha contraria o un espacio vacio
         (if (or (equal? (string-ref str endPos) #\x ) (= (colorComprobant endPos str) -1 ))
             
             ;;En caso de que esto sea verdad entonces
             ;;Esta condicion hara el recorrido por la string corroborando que el camino esta libre
             (if (or (equal? (string-ref str (+ inicialPos stop)) #\x) (equal? (colorComprobant endPos str) -1 ))
                 ;;Esta es la condicion de parada
                 (if (= stop directionLine)
                     #t
                     (nonJump (- stop directionLine))
                     );;EndIf
                  #f
                 )
             #f 
          );;EndIf;;Else
         
         ;;Este recorrido se refiere a las fichas blancas
         (if (or (equal? (string-ref str endPos) #\x ) (= (colorComprobant endPos str) 1 ))

             ;;Esta condicion hara el recorrido por la string corroborando que el camino esta libre
             (if (or (equal? (string-ref str (+ inicialPos stop)) #\x) (= (colorComprobant endPos str) 1 ))
                 ;;Esta es la condicion de parada
                 (if (= stop directionLine)
                     #t
                     (nonJump (- stop directionLine))
                     );EndIf
                  #f
                 )
             #f 
          );endIf 
     );EndIf
    );EndDefine nonJump
  
  
  ;;Esta función determinara si una pieza puede hacer un movimiento en linea
  ;;La logica se basa en que para que un movimiento sea lineal entonces
  ;;En las posiciones x o y solo se modificara uno de los dos valores

  (define (inMoveLine )

    ;;Esta condicion revisara si la ficha seleccionada se puede mover recto o no 
    (if (or (equal? (string-ref str inicialPos) #\T ) (equal? (string-ref str inicialPos) #\t );;Aqui evaluamos las torres
            (equal? (string-ref str inicialPos) #\D ) (equal? (string-ref str inicialPos) #\d );;Aqui evaluamos las damas
            (equal? (string-ref str inicialPos) #\R ) (equal? (string-ref str inicialPos) #\r );;Aqui evaluamos el rey 
            (equal? (string-ref str inicialPos) #\P ) (equal? (string-ref str inicialPos) #\p );;Aqui evaluamos los peones
            );;EndCondition
        (if (nonJump distance)
            "El movimiento es recto"
            "El movimiento no es recto"
            );;EndIf
        "Esta ficha no se puede mover de forma recta"
        );;EndIf

    );EndFunction inMoveLine

  (inMoveLine)
  
  );;------------------endFunction restrictions-----------------;;

;;Coronacion

(define (coronation actPos str)
  
  ;Este identificador va a guardar las iniciales de las fichas Negras
  (define selectStringB (string-append "T" "C" "A" "D" ))
  ;Este identificador va a guardar las iniciales de las fichas Blancas
  (define selectStringW (string-append "t" "c" "a" "d" ))
     
  ;;Esta funcion solo sirve para poner las fichas en el lateral izquierdo al momento de querer seleccionar el cambio de ficha 
  (define (PiecePutOnSelection str count)
    (define x 525)
    (define y (+ 150 (* count 50)))
    
    ;;Si el contador es menor al tamaño de la string que llamamos 
    (if (< count (string-length str))
        
        (begin
          ((draw-rectangle ventana1) (make-posn 500 150) 100 200)
          (((draw-pixmap-posn
             (PieceAsignament (string-ref str count)))
            ventana1)
           (make-posn x y))
          (PiecePutOnSelection str (+ count 1))
          );endBegin;;else
        (PiecePutOnSelection str (+ count 1))      
        );end (if (< count (string-length str))
    ); EndFunction PiecePutOnSelection

  ;;En caso de que el peon Blanco se encuentre en el rango va a llamar la funcion de seleccion de ficha
  ;; "selectStringW"
  (if (and (>= actPos 0) (<= actPos 7) )
       
      (PiecePutOnSelection selectStringW 0)

      ;;En caso contrario, si el peon negro se encuentra en rango, va a llamar la funcio de seleccion de ficha
      ;; "selectStringB"
      (if (and (>= actPos 56) (<= actPos 63))
          
          (PiecePutOnSelection selectStringB 0)
     
          "Aun no esta en coronacion" 

          ) ;EndIf (and (>= (pawnMovement) 56) (<= (pawnMovement) 63)...#\P))
      ); EndIf (and (>= (pawnMovement) 0) (<= (pawnMovement) 7)...#\p))
  );EndFunction Coronation


;;------------------StarFunction generalMove-----------------;;

;;generalMove: Esta función tomara una string y el primer click que se da, esperando que sea de la ficha correspondiente
;;para poder generar el movimiento adecuado

(define (generalMove oldString oldPos turn)
  
  (define click (mouse-click-posn (get-mouse-click ventana1)))
  (define comprobada (limites click) )
  (define pos-xNew (pos-x comprobada))
  (define pos-yNew (pos-y comprobada))
  (define newPos (casilla pos-xNew pos-yNew))
  (define makeMove (MakeString oldString (string-ref oldString oldPos) oldPos newPos))
  (printf "turnodoooos ~a"pos-xNew)

  ;;------------------StartFunctionMovements-----------------;;

  ;;Esta funcion tomaras posibilidades de hacer un movimiento, para mas tarde utilizarla en los jaques

(define (movements?)

;;------------------StartFunction pawnMovements-----------------;;

  ;;Aquí definimos la funcion del movimiento del peon no toma valores, pues ya los tomamos en la funcion inicial (generalMove)

  (define (pawnMovements?)

    ;;Esta condicion de aqui, es si el turno es equivalente a 1 (es decir, se mueven las fichas negras)
    (if (equal? turn 1 )
      
        (if ( and ( >= oldPos 8 ) ( <= oldPos 15 ) (equal? (string-ref oldString oldPos) #\P) (= oldPos (- newPos 16))
                  (equal? (string-ref oldString (- newPos 8)) #\x) (equal? (string-ref oldString newPos) #\x))
            #t
            (if  (and (equal? (string-ref oldString oldPos) #\P) (= oldPos (- newPos 8))
                      (equal? (string-ref oldString newPos) #\x) (coronation newPos oldString))
                
                 #t
                 (if (and (equal? (string-ref oldString oldPos) #\P) (or (= oldPos (- newPos 7)) (= oldPos (- newPos 9)))
                          (equal? (colorComprobant newPos oldString) -1 ) (coronation newPos oldString)) 
                     #t
                     #f
                     );;EndIf
                 );EndIf
            );EndIf

        ;;Esta condicion de aqui, es si el turno es equivalente a -1 (es decir, se mueven las fichas blancas)
        (if ( and ( >= oldPos 48 ) ( <= oldPos 55 ) (equal? (string-ref oldString oldPos) #\p) (= oldPos (+ newPos 16))
                  (equal? (string-ref oldString (+ newPos 8)) #\x) (equal? (string-ref oldString newPos) #\x))
            #t
            (if  (and (equal? (string-ref oldString oldPos) #\p) (= oldPos (+ newPos 8))
                      (equal? (string-ref oldString newPos) #\x) (coronation newPos oldString))
                 #t
                 (if (or (and (equal? (string-ref oldString oldPos) #\p) (or (= oldPos (+ newPos 7)) (= oldPos (+ newPos 9)))
                          (equal? (colorComprobant newPos oldString) 1 )) (coronation newPos oldString) ) 
                     #t
                     #f
                     );;EndIf
                 );EndIf
            );;EndIf
        );EndIf
  
  );;------------------EndFunction pawnMovements-----------------;;

  ;;Esta condicion, es para evitar que el usuario clickee en la misma posicion y pierda su turno,
  ;;Si la posicion anterior es diferente a la posicion actual, entonces llamaremos a pawnMovements
  ;;(inicialmente debe de llamar a los movimientos en general)
  (if (= oldPos newPos)
      #f
     (pawnMovements?))
  
 );;------------------EndFunction movements?-----------------;;

  ;;Esta comparacion, ve lo que devuelve (movements?) recordemos que esta funcion solo devuelve #t o #f
  ;;En caso de que sea #t entonces observara si el makeMove no es igual a oldString esto para poder mostrar el movimiento
  ;;En pantalla y llamar a (turns) pero multiplicandolo por -1 para que devuelva el turno contrario al que ya jugó
  (if (movements?)
      (if (not (equal? makeMove oldString))
          (begin
            (table makeMove)
            (turns makeMove (* -1 turn )))
          ;;Si son iguales entonces llama a turns sin modificar ni el tablero ni el turno
          (turns oldString turn)
          );EndIf
      ;;Si (movements?) es falso entonces llama a turns sin modificar ni el tablero ni el turno
      ;;supongo que esto realmente debera llamar a la funcion de jaque mate...
      (turns oldString turn)
      );endIf
      
  
  );;------------------EndFunction generalMove-----------------;;

;;------------------StartFunction turns-----------------;;

;;turns: La funcion turns recibira una string (que sera la que se modificara EN CASO de que se deba modificar)
;;Y recibira el turno (que se debera modificar EN CASO de que la jugada haya sido valida
(define (turns string turn)
  
  (define click (mouse-click-posn (get-mouse-click ventana1)))
  (define comprobada (limites click) )
  (define pos-xPiece (pos-x comprobada))
  (define pos-yPiece (pos-y comprobada))
  (define piecePos (casilla pos-xPiece pos-yPiece))
  ;;piece: guardara la informacion que devuelva colorComprobant
  ;;(recordemos que colorComprobant devuelve -1 o 1)
  (define piece (colorComprobant piecePos string))

 ;;whiteTurn evaluara si la pieza a la que nos referimos es blanca
 ;;Mostrara en pantalla un recuadro que dira si mueven las blancas
 ;;EN CASO de que no clickee una ficha blanca mostrara un recuadro
 ;;diciendo "no es una ficha blanca" y no perdera el turno
  
  (define (whiteTurn)
    
    (if (equal? piece -1)
          (begin
            (printf "mueven las blancas\n")
            (generalMove string piecePos turn)
            );EndBegin;else
        (begin
          (printf "no es una ficha blanca\n")
          (turns string turn)
          );endBegin 
        );EndIf
    
    );endDefine whiteTurn

  ;;blackTurn evaluara si la pieza a la que nos referimos es negra
 ;;Mostrara en pantalla un recuadro que dira que mueven las negras
 ;;EN CASO de que no clickee una ficha negra mostrara un recuadro
 ;;diciendo "no es una ficha negra" y no perdera el turno
  
  (define (blackTurn)
    
    (if (equal? piece 1)
        (begin
          (printf "mueven las negras\n")
          (generalMove string piecePos turn)
          );endBegin
        (begin
          (printf "no es una ficha negra\n")
          (turns string turn)
          );;endBegin
        );;EndIf
    
    );;EndDefine blackTurn

  ;;Lo primero que hará la funcion turns es evaluar si "turn" es igual a -1
  ;;si es #t entonces llamara la funcion (whiteTurn)
  ;;(recordemos que siempre empieza la partida moviendo las fichas blancas)
  ;;En caso contrario llamara a (blackTurn)
  (if (equal? turn -1)
      (whiteTurn)
      (blackTurn)
      );EndIf
  
  );;------------------EndFunction turns-----------------;;



(turns inicialTable -1)


 
