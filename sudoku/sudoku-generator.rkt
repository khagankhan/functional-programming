#lang racket

;; Shuffle function for lists
(define (shuffle lst)
  (let loop ([lst lst] [result '()])
    (if (null? lst)
        result
        (let-values ([(new-lst elt) (remove-random lst)])
          (loop new-lst (cons elt result))))))

;; Remove a random element from a list
(define (remove-random lst)
  (let ([idx (random (length lst))])
    (values (remove* (list (list-ref lst idx)) lst)
            (list-ref lst idx))))

;; Function to create a filled 3x3 sudoku grid
(define (create-filled-grid)
  (let ([base-row (shuffle (list 1 5 9 7 8 2 3 4 6))])
    (for/list ([i (in-range 9)])
      (for/list ([j (in-range 9)])
        (list-ref base-row (modulo (+ i j) 9))))))

;; Function to get subgrid indices
(define (subgrid-indices i j)
  (let ([x (* 3 (quotient i 3))]
        [y (* 3 (quotient j 3))])
    (for*/list ([dx (in-range 3)] [dy (in-range 3)])
      (list (+ x dx) (+ y dy)))))

;; Function to check and zero if duplicate
(define (zero-if-duplicate grid val i j)
  (let ([indices (subgrid-indices i j)])
    (if (> (count (lambda (coords)
                    (= val (list-ref (list-ref grid (second coords)) (first coords))))
                indices)
           1)
        0
        val)))

;; Function to zero out duplicates in each subgrid
(define (zero-duplicates grid)
  (for/list ([i (in-range 9)])
    (for/list ([j (in-range 9)])
      (zero-if-duplicate grid (list-ref (list-ref grid i) j) i j))))

;; Generate and print a 3x3 Sudoku puzzle
(define (print-grid grid)
  (for ([row grid])
    (for ([num row])
      (printf "~a " num))
    (printf "\n")))

;; Print grid with no spaces between digits
(define (print-grid-no-spaces grid)
  (for ([row grid])
    (for ([num row])
      (printf "~a" num))
    (printf "\n")))

;; Main function to generate and print a Sudoku puzzle
(define (generate-and-print-3x3-sudoku)
  (let ([filled-grid (create-filled-grid)])
    (printf "Original Grid:\n")
    (print-grid filled-grid)
    (printf "\nZeroed Grid (no spaces):\n")
    (print-grid-no-spaces (zero-duplicates filled-grid))))

;; Run the generator and print the puzzle
(generate-and-print-3x3-sudoku)

;; TODO: It generates sudoku with the same structure. Needs to be more diversed in the future