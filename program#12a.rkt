(define (getCell matrix row column)
      (if (> row 1)
          (getCell (cdr matrix) (- row 1) column)
          (getColumn (car matrix) column)
          )
      )


(define (getColumn matrix column)
  (if (> column 1)
      (getColumn (cdr matrix) (- column 1))
      (car matrix)
      )
  )

(define (setCell matrix row column item)
  (if (> row 1)
      (cons (car matrix) (setCell (cdr matrix) (- row 1) column item))
      (cons (setColumn (car matrix) column item) (cdr matrix))
      )
  )


(define (setColumn matrix column item)
  (if (> column 1)
      (cons (car matrix) (setColumn (cdr matrix) (- column 1) item))
      (cons item (cdr matrix))
      )
  )

