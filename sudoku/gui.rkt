#lang racket
(require racket/draw)
(require "./sudoku-solver.rkt")

(define CELL-SIZE 30)

(define (draw-centered-text dc text x y)
  (let-values ([(w h d s) (send dc get-text-extent text)])
    (let ([x (+ x (/ (- CELL-SIZE w) 2))]
          [y (+ y (/ (- CELL-SIZE h d) 2))])
      (send dc draw-text text x y ))))

(define (draw-puzzle p1 p2)
  (let* ([drawing (make-bitmap (* 9 CELL-SIZE) (* 9 CELL-SIZE))]
         [dc (new bitmap-dc% [bitmap drawing])]
         [yellow (new brush% [color (make-object color% 240 210 0)])]
         [gray (new brush% [color "Gainsboro"])])
    (for* ([r 9][c 9])
      (let* ([x (* c CELL-SIZE)]
             [y (* r CELL-SIZE)]
             [n1 (send p1 item-ref r c)]
             [n2 (send p2 item-ref r c)]
             [num (if (zero? n2) "" (number->string n2))]
             [color (if (zero? n1) yellow gray)])
        (send dc set-pen "black" 1 'solid)
        (send dc set-brush color)
        (send dc draw-rectangle x y CELL-SIZE CELL-SIZE)
        (draw-centered-text dc num x y)))
    (for* ([r 3][c 3])
      (let* ([x (* 3 c CELL-SIZE)]
             [y (* 3 r CELL-SIZE)])
        (send dc set-pen "black" 2 'solid)
        (send dc set-brush "black" 'transparent)
        (send dc draw-rectangle x y (* 3 CELL-SIZE) (* 3 CELL-SIZE))))
    drawing))

(define (solve-gui pstr)
  (let* ([puzzle (new sudoku% [puzzle-string pstr])]
         [solution (solve-sudoku puzzle)])
    (print (draw-puzzle puzzle puzzle))
    (newline)
    (newline)
    (print (draw-puzzle puzzle solution))))

(define medium
"200080300
 060070084
 030500209
 000105408
 000000000
 402706000
 301007040
 720040060
 004010003")

(solve-gui medium)