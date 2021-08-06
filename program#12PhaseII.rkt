(define (JWAgetCell matrix row column)
      (if (> row 1)
          (JWAgetCell (cdr matrix) (- row 1) column)
          (JWAgetColumn (car matrix) column)
          )
      )


(define (JWAgetColumn matrix column)
  (if (> column 1)
      (JWAgetColumn (cdr matrix) (- column 1))
      (car matrix)
      )
  )

(define (JWAsetCell matrix row column item)
  (if (> row 1)
      (cons (car matrix) (JWAsetCell (cdr matrix) (- row 1) column item))
      (cons (JWAsetColumn (car matrix) column item) (cdr matrix))
      )
  )


(define (JWAsetColumn matrix column item)
  (if (> column 1)
      (cons (car matrix) (JWAsetColumn (cdr matrix) (- column 1) item))
      (cons item (cdr matrix))
      )
  )


(define JWAGame 0)

(define (JWAStartGame)
  (begin
    (set! JWAGame '( 1 (
                    ( 0 0 0 0 0 0 0)
                    ( 0 0 0 0 0 0 0)
                    ( 0 0 0 0 0 0 0)
                    ( 0 0 0 0 0 0 0)
                    ( 0 0 0 0 0 0 0)
                    ( 0 0 0 0 0 0 0))))
    (display " Hello There! This is my AI \n")
    #t
    )
  )



(define (JWAMarkMove col)
  (begin
    (set! JWAGame
     (if (JWALegalMoveP col)
         (cons (JWANextPlayer) (cons (JWAsetCell (car (cdr JWAGame)) (JWAGetEmptyRow (car (cdr JWAGame)) 1 col) col (car JWAGame)) ()))
         JWAGame
            )
    )
    col
    )
  )
        
(define (JWANextPlayer)
  (if ( = (car JWAGame) 1)
      2
      1
      )
  )
  
                 

(define (JWAMakeMove)
  (JWAMarkMove
   (JWAChooseMove(JWARandomMove))
           )
   )

(define (JWARandomMove)
  (+ 1 (random 7))
  )


(define (JWAChooseMove col)
  ;(JWAsetCell (car (cdr JWAGame)) (getEmptyRow (car (cdr JWAGame)) col 1) col 1)
  (if (JWALegalMoveP col)
      col
      (JWAChooseMove (+ 1 (random 7) ))
      )
  )
  


 (define (JWAGetEmptyRow matrix row col)
       (if (equal? 0 (JWAgetCell matrix row col))
           row
           (JWAGetEmptyRow matrix (+ row 1) col)
       )
   )
             

(define (JWAShowGame)
  (begin
    (display (car (cdr (cdr (cdr (cdr (cdr (car (cdr JWAGame)))))))))
    (newline)
    (display (car (cdr (cdr (cdr (cdr (car (cdr JWAGame))))))))
    (newline)
    (display (car (cdr (cdr (cdr (car (cdr JWAGame)))))))
    (newline)
    (display (car (cdr (cdr (car (cdr JWAGame))))))
    (newline)
    (display (car (cdr (car (cdr JWAGame)))))
    (newline)
    (display (car (car (cdr JWAGame))))
    (newline)
    (display (car JWAGame))
    (newline)
    #t
    )
  )

(define (JWALegalMoveP col)
  (if (and (>= col 1) (<= col 7))
      #t
      #f
      )
  )


(define (JWAWinP col)
  (if (= (JWAHorizontalWin (car (cdr JWAGame)) (- (JWAGetEmptyRow (car (cdr JWAGame)) 1 col) 1) col (car JWAGame)) 4)
      #t
      (if (= (JWAVerticalWin (car (cdr JWAGame)) (- (JWAGetEmptyRow (car (cdr JWAGame)) 1 col) 1) col (car JWAGame)) 4)
          #t
          (if (= (JWADiagonalRightWin (car (cdr JWAGame)) (- (JWAGetEmptyRow (car (cdr JWAGame)) 1 col) 1) col (car JWAGame)) 4)
              #t
              (if (= (JWADiagonalLeftWin (car (cdr JWAGame)) (- (JWAGetEmptyRow (car (cdr JWAGame)) 1) 1 col) col (car JWAGame)) 4)
                  #t
                  #f
                  )
              )
          )
      )
  )
        
  

(define (JWASumVector matrix row col item drow dcol)
  (if (or (> row 6 ) (< row 1 ) (< col 1) (> col 7))
      0
      (if (= item (JWAgetCell matrix row col))
          ( + 1 (JWASumVector matrix (+ row drow) (+ col dcol) item drow dcol))
          0
          )
      )
  )
  
(define (JWAHorizontalWin matrix row col item)
  (+ 1 (JWASumVector matrix row (- col 1) item 0 -1)
     (JWASumVector matrix row (+ col 1) item 0 1 ))
  )

(define (JWAVerticalWin matrix row col item)
  (+ 1 (JWASumVector matrix (- row 1) col item -1 0)
     (JWASumVector matrix (+ row 1) col item 1 0)
     )
  )

(define (JWADiagonalRightWin matrix row col item)
  (+ 1 (JWASumVector matrix (+ row 1) (+ col 1) item 1 1)
     (JWASumVector matrix (- row 1) (- col 1) item -1 -1)
     )
  )
(define (JWADiagonalLeftWin matrix row col item)
  (+ 1 (JWASumVector matrix (- row 1) (+ col 1) item -1 1)
     (JWASumVector matrix (+ row 1) (- col 1) item 1 -1)
     )
  )
  
(define (JWAWillWinP col)
  (if (= (JWAHorizontalWin (car (cdr JWAGame)) (JWAGetEmptyRow (car (cdr JWAGame)) 1 col) col (car JWAGame)) 4)
      #t
      (if (= (JWAVerticalWin (car (cdr JWAGame)) (JWAGetEmptyRow (car (cdr JWAGame)) 1 col) col (car JWAGame)) 4)
          #t
          (if (= (JWADiagonalRightWin (car (cdr JWAGame)) (JWAGetEmptyRow (car (cdr JWAGame)) 1 col) col (car JWAGame)) 4)
              #t
              (if (= (JWADiagonalLeftWin (car (cdr JWAGame)) (JWAGetEmptyRow (car (cdr JWAGame)) 1 col) col (car JWAGame)) 4)
                  #t
                  #f
                  )
              )
          )
      )
  )