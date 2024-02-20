#lang racket

(require json)

;; Strucutres for cards, players, and the game state
(struct card (name uses) #:transparent)
(struct player (coins buys cards) #:transparent)
(struct game-state (day phase shop players player) #:transparent)

;; Parse a JSON string into game state structure
(define (parse-game-state json-string)
  (let ((json (string->jsexpr json-string)))
    (game-state
     (hash-ref json 'day)
     (parse-phase (hash-ref json 'phase))
     (hash-ref json 'shop)
     (map parse-player (hash-ref json 'players))
     (hash-ref json 'player))))

;; Debug Print functions:

#|
(define (print-player-state player)
  (printf "Player Coins: ~a, Buys: ~a, Cards: ~a\n"
          (player-coins player)
          (player-buys player)
          (map card-name (player-cards player))))

(define (print-game-state state)
  (printf "Day: ~a, Phase: ~a\n" (game-state-day state) (hash-ref (game-state-phase state) 'name))
  (for-each print-player-state (game-state-players state))
  (printf "Shop Inventory: ~a\n" (game-state-shop state)))
|#

;; Parse a player from JSON
(define (parse-player json-player)
  (player
   (hash-ref json-player 'coins)
   (hash-ref json-player 'buys)
   (map parse-card (hash-ref json-player 'cards))))

;; Parse a card from JSON
(define (parse-card json-card)
  (card
   (hash-ref json-card 'name)
   (hash-ref json-card 'uses)))

;; Parse the phase from JSON
(define (parse-phase json-phase)
  (cond
    [(string=? (hash-ref json-phase 'name) "investing") json-phase]
    [(string=? (hash-ref json-phase 'name) "attacking") json-phase]
    [(string=? (hash-ref json-phase 'name) "buy") json-phase]
    [(string=? (hash-ref json-phase 'name) "end" (exit)) json-phase] ;; exit gracefully when done (Checked with the last json)
    [else (error "[-] Error: Unknown phase or invalid JSON")]))

;; I thought changing symbol->string can be a side effect. So for buy phase I keep them as symbol from the beginning.

(define (card-cost card-name)
  ;; (printf "card-cost called with: ~a\n" card-name) -> Debug
  (let ((cost (cond
                [(equal? card-name (string->symbol "Board of Monopoly")) 2]
                [(equal? card-name (string->symbol "Bubble")) 2]
                [(equal? card-name (string->symbol "Ghost")) 2]
                [(equal? card-name (string->symbol "Gold Fish")) 3]
                [(equal? card-name (string->symbol "Thug")) 3]
                [(equal? card-name (string->symbol "Incantation")) 4]
                [(equal? card-name (string->symbol "Shield of Greed")) 4]
                [(equal? card-name (string->symbol "Magic Bean Stock")) 1]
                [(equal? card-name (string->symbol "Senior Worker")) 2]
                [(equal? card-name (string->symbol "Worker")) 1]
                [(equal? card-name (string->symbol "Apprentice")) 1]
                [(equal? card-name (string->symbol "Golem")) 5]
                [(equal? card-name (string->symbol "Wall of Wealth")) 1]
                [else 0]))) ;; Default
  ;; (printf "Cost of ~a: ~a\n" card-name cost) -> Debug
  cost))
;; (card-cost (string->symbol "Bubble")) -> Debug

(define (card-attack card-name)
  (match card-name
    ["Magic Bean Stock" 1]
    ["Worker" 1]
    ["Senior Worker" 2]
    ["Board of Monopoly" 1]
    ["Incantation" 1]
    ["Wall of Wealth" 1]
    ["Ghost" 3]
    ["Bubble" 9]
    ["Gold Fish" 1]
    ["Apprentice" 2]
    ["Thug" 4]
    ["Shield of Greed" 2]
    ["Golem" 7]
    [else 0]))

(define (card-defense card-name)
  (match card-name
    ["Magic Bean Stock" 1]
    ["Worker" 2]
    ["Senior Worker" 2]
    ["Board of Monopoly" 1]
    ["Incantation" 1]
    ["Wall of Wealth" 2]
    ["Ghost" 2]
    ["Bubble" 2]
    ["Gold Fish" 2]
    ["Apprentice" 1]
    ["Thug" 4]
    ["Shield of Greed" 7]
    ["Golem" 7]
    [else 0]))

;; Simple logic to decide if a card should attack
(define (should-attack? card-name)
  (> (card-attack card-name) 2)) ;; this can be adjusted I choose 2 for now

;; Choose the best card to attack with based on attack value
;; Find the best card to attack with
(define (best-attack-card cards)
  (let loop ((remaining cards)
             (best-card '())
             (highest-attack -1))
    (if (null? remaining)
        best-card ;; Return the best card found
        (let* ((current-card (first remaining))
               (current-attack (card-attack (card-name current-card))))
          (if (> current-attack highest-attack)
              (loop (rest remaining) current-card current-attack) ;; Update best card
              (loop (rest remaining) best-card highest-attack)))))) ;; Continue with the same best card


(define (generate-attacking-move state)
  (let* ((current-player (list-ref (game-state-players state) (game-state-player state)))
         (actionable-cards (filter (lambda (c) (and (= (card-uses c) 0) 
                                                    (should-attack? (card-name c))
                                                    (not (equal? (card-name c) "Sorcerer's Stipend")))) ;; Needed to be improved or no need this line at all
                                   (player-cards current-player)))
         (best-card (best-attack-card actionable-cards)))
    (if best-card
        (list (+ 1 (index-of (player-cards current-player) best-card)))
        (list 0))))

(define (index-of lst element)
  (define (loop lst idx)
    (cond
      [(null? lst) -1]
      [(equal? (first lst) element) idx]
      [else (loop (rest lst) (add1 idx))]))
  (loop lst 0))

;; increment each player's coins by 1 for Sorcerer's Stipend
(define (apply-sorcerers-stipend players)
  (map (lambda (p) (player (+ (player-coins p) 1)
                           (player-buys p)
                           (player-cards p)))
       players))

;; Function to update the game state for a new day, including Sorcerer's Stipend effect
(define (update-game-state-for-new-day state)
  (game-state (game-state-day state)
              (game-state-phase state)
              (game-state-shop state)
              (apply-sorcerers-stipend (game-state-players state))
              (game-state-player state)))


(define (generate-move state)
  ;; Debug Game States
  ;; (printf "Before Move:\n")
  ;; (print-game-state state)
  (let* ((updated-state (if (or (equal? (hash-ref (game-state-phase state) 'name) "investing") (equal? (hash-ref (game-state-phase state) 'name) "buy")) ;; let* can be used immediately
                            (update-game-state-for-new-day state)
                            state))
         (phase (game-state-phase updated-state))
         ;; Store the result of move generation for debugging or further processing
         (move-result (cond
                        [(string=? (hash-ref phase 'name) "investing") (generate-investing-move updated-state)]
                        [(string=? (hash-ref phase 'name) "attacking") (generate-attacking-move updated-state)]
                        [(string=? (hash-ref phase 'name) "buy") (generate-buying-move updated-state)]
                        [(string=? (hash-ref phase 'name) "end") (exit 0)] ; Handle end phase
                        [else (error "Unknown phase")])))
  
   ;; Debug Game States
   ;; (printf "After Move:\n")
   ;; (print-game-state updated-state)
  move-result))

;; Generates a move for the investing phase with a simple strategy
(define (generate-investing-move state)
  (let ((coins (player-coins (list-ref (game-state-players state) (game-state-player state)))))
    (list (max 0 (- coins 1))))) ;; Keep 1 coin if possible then keep investing

;; Generates a move for the buy phase with a simple strategy
;; If you have more coin than 2 buy. Buy the cheapest -> Very dumb strategy keep for now
(define (generate-buying-move state)
  (let* ((current-player (list-ref (game-state-players state) (game-state-player state)))
         (coins (player-coins current-player))
         (shop-list (hash->list (game-state-shop state)))
         (affordable-cards (filter (λ (card-pair)
                                     (and (> (cdr card-pair) 2) ;; Card is available
                                          (<= (card-cost (car card-pair)) coins))) ;; Player can buy
                                   shop-list)))
    (if (null? affordable-cards)
        (list "Pass")
        (let ((card-to-buy (car (sort affordable-cards (λ (a b) (< (card-cost (car a)) (card-cost (car b))))))))
          (list (symbol->string (car card-to-buy)))))))

;; I/O -> Side effect but no other choice
(define (get-and-parse-state)
  ;; (display "Enter the game state in JSON format:\n")
  (flush-output)
  (let ((input (read-line)))
    (parse-game-state input)))

(define (run-game-loop)
  (let loop ((state (get-and-parse-state)))
    (let ((move (generate-move state)))
      (displayln (jsexpr->string move))
      (unless (string=? (hash-ref (game-state-phase state) 'name) "end")
        (loop (get-and-parse-state))))))

(run-game-loop)