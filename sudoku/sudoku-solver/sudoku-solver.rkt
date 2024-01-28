;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Project 1: Functional Programming. Sudoku solver ;;
;;               -*- mode: Racket -*-               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket
(require racket/draw)
(provide (all-defined-out))

(define (array-set! array row col vec)
  (vector-set! (vector-ref array row) col vec))

(define (array-ref array row col)
  (vector-ref (vector-ref array row) col))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function to get block based on the row and column: block-get!
(define (block-get! row col)
  ( + (* 3 (quotient row 3)) (quotient col 3)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make markers
(define (make-markers)
  (for/vector ([i 10])
    (let ([v (make-vector 10 #t)])
      (vector-set! v 0 #f)
      v)))
;; Thanks to available function we can create a list of available numbers
(define (free-numbers v)
  (for/list ([num (in-range 1 10)] #:when (vector-ref v num)) num))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; We use sudoku object to keep track and maintain the state of puzzle.
(define sudoku%
  (class object%

  (init [puzzle-string ""])
    
    (define available-row (make-markers))
    (define available-column (make-markers))
    (define available-block (make-markers))
  (define count 0)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
  (define grid
      (for/vector ([i 9]) (make-vector 9 0)))
    (super-new)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define/public (item-set! r c n)
      (array-set! grid r c n)
      (array-set! available-row r n #f)
      (array-set! available-column c n #f)
      (let ([b (block-get! r c)])
        (array-set! available-block b n #f))
      (set! count (+ count 1)))
    (unless (equal? puzzle-string "")
    (init-puzzle puzzle-string))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define/public (get-grid) grid)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    (define/public (item-ref r c)
      (array-ref grid r c))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define/public (init-grid grid)
      (for* ([r 9] [c 9])
        (let ([n (array-ref grid r c)])
          (when (> n 0)
            (item-set! r c n)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;            
  (define/private (init-puzzle p)
      (let ([g 
             (let ([rows (string-split p)])
               (for/vector ([row rows])
                 (for/vector ([c 9])
                   (string->number (substring row c (add1 c))))))])
        (init-grid g)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define/public (set-all-singles)
  (when (set-singles) (set-all-singles)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define/public (get-min-free)
  (let ([min-free 10]
        [min-info null]
        [free-list (get-free)])
    (let loop ([free free-list])
      (unless (equal? free '())
        (let* ([info (car free)]
               [rem (cdr free)]
               [num-free (third info)])
              (when (< 0 num-free min-free)
                (set! min-free num-free)
                (set! min-info info))
              (loop rem))))
    min-info))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define/public (solved?) (= count 81))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define/public (clone)
  (let ([p (new sudoku%)])
    (send p init-grid grid)
    p))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (available row col)
  (let* ([block (block-get! row col)]
         [ar (vector-ref available-row row)]
         [ac (vector-ref available-column col)]
         [ab (vector-ref available-block block)])
    (for/vector ([i 10])
      (and (vector-ref ar i)
           (vector-ref ac i)
           (vector-ref ab i)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (set-singles)
  (let ([found #f])
    (for* ([row 9] [col 9])
      (let* ([free (available row col)]
             [num-free (vector-count identity free)]
             [num (item-ref row col)])
        (when (and (zero? num) (= 1 num-free))
          (let ([first-free
                 (let loop ([i 1])
                   (if (vector-ref free i) i
                       (loop (add1 i))))])
            (item-set! row col first-free)
            (set! found #t))
          )))
    found))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (get-free)
  (let ([free-list '()])
    (for* ([row 9] [col 9])
      (let* ([free (available row col)]
             [num-free (vector-count identity free)]
             [num (item-ref row col)])
        (when (zero? num)
          (set! free-list
                (cons
                 (list row col num-free (free-numbers free))
                 free-list)))))
    free-list))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define/public (print)
  (for* ([row 9] [col 9])
    (when (zero? col)(printf "\n"))
    (let ([num (item-ref row col)])
      (if (zero? num)
          (printf " •")
          (printf " ~a" num)
          )))
  (printf "\n"))
))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (solve-sudoku puzzle)
  (let ([solution null]
        [puzzle (send puzzle clone)])
    (define (dfs puzzle)
      (if (send puzzle solved?)
          (set! solution puzzle)
          (let ([info (send puzzle get-min-free)])
            (match info
              ['() #f]
              [(list row col num free-nums)
               (let loop ([nums free-nums])
                 (if (equal? nums '())
                     #f
                    (let ([n (car nums)]
                          [t (cdr nums)])
                          (let ([p (send puzzle clone)])
                            (send p item-set! row col n)
                            (send p set-all-singles)
                           (unless (dfs p) (loop t))))))]))))
      (send puzzle set-all-singles)
      (dfs puzzle)
      (if (equal? solution null)
          (error "[-] Error: No solution exists or found...")
          solution
          )))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (solve pstr)
  (let* ([puzzle (new sudoku% [puzzle-string pstr])]
         [solution (solve-sudoku puzzle)])
    (if solution
        (begin
          (send puzzle print) 
          (send solution print))
        (error "[-] Error: Multiple or no solution"))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO: CANNOT SOLVE MULTIPLE SOLUTIONS OR NO SOLUTIONS YET