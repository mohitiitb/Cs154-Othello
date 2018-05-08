#lang racket/gui
;[Developed by Mohit Gupta and Manas Shukla]
;[basic function ]

(define (is-present? x l)
  (cond  [(null? l) #f]
         [(equal? (car l) x) #t]
         [else (is-present? x (cdr l))]))

;[macros for loop]
(define-syntax for
  (syntax-rules (:)
    [(for init ... : condition : step : statements)
     (begin init ...
            (define (iter)
              (cond [condition (begin statements
                                      step
                                      (iter))]))
            (iter))]))


;[Definition of 2d vectors]

(define (make-2d-vector r c initial)
  (build-vector r (lambda (x) (make-vector c initial))))

(define (2d-vector-ref vec r c)
  (vector-ref (vector-ref vec r) c))

(define (2d-vector-set! vec r c new-val)
  (vector-set! (vector-ref vec r) c new-val))

(define (2d-vector-copy vec)
  (build-vector (vector-length vec) (lambda (x) (vector-copy (vector-ref vec x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;[CODING PART OF GAME];;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;[Human VS Human];;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Representation board as 8*8 2d vector 
(define board (make-2d-vector 8 8 'x))

#|(define board (list->vector (list (list->vector (list 'x 'x 'x 'x 'x 'x 'x 'x))
                                  (list->vector (list 'x 'x 'x 'x 'x 'x 'x 'x))
                                  (list->vector (list 'x 'x 0 'x 'x 'x 'x 'x))
                                  (list->vector (list 'x 'x 1 0 0 0 'x 'x))
                                  (list->vector (list 'x 'x 'x 1 1 'x 'x 'x))
                                  (list->vector (list 'x 'x 'x 'x 1 'x 'x 'x))
                                  (list->vector (list 'x 'x 'x 'x 'x 'x 'x 'x))
                                  (list->vector (list 'x 'x 'x 'x 'x 'x 'x 'x))
                                
                                
                                )))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;[displays board]

(define (print board)
  (define l (vector->list board))
  (define (helper)
    (cond[(not (null? l)) (begin (display (car l))
                                 (newline)
                                 (set! l (cdr l))
                                 (helper)
                                 )]))
  (helper))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (create-temp bd)
  (define temp (make-2d-vector 8 8 'e))
  (define (iter r c)
    (cond[(< r 8) (begin  
                    (2d-vector-set! temp r c (2d-vector-ref bd r c))
                    (if (= c 7) (iter (+ r 1) 0)
                        (iter r (+ c 1))))]))
  (iter 0 0)
  temp)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;1 stands for black-coin and 0 stands for white-coin
(define (change coin)
  (cond[(equal? coin 0) 1]
       [else 0]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (count-points bd)   ;counts the number of black and white coins in the board 
  (define black-points 0)   ;returns ans as (black . white )
  (define white-points 0)
  (define (helper r c)
    (if (> r 7) (cons black-points white-points) (let*([x (2d-vector-ref bd r c)])
                                                   (if (= c 7) (if (number? x) 
                                                                   (if (= x 0) (begin (set! white-points (+ white-points 1))
                                                                                      (helper (+ r 1) 0))
                                                                       (begin (set! black-points (+ black-points 1))
                                                                              (helper (+ r 1) 0))
                                                                       
                                                                       )
                                                                   (helper (+ r 1) 0))
                                                       (if (number? x) 
                                                           (if (= x 0) (begin (set! white-points (+ white-points 1))
                                                                              (helper r (+ c 1)))
                                                               (begin (set! black-points (+ black-points 1))
                                                                      (helper r (+ c 1)))
                                                               
                                                               )
                                                           (helper r (+ c 1))
                                                           )
                                                       
                                                       )
                                                   ))
    )
  (helper 0 0)
  )
(define (count-black bd)
  (car (count-points bd)))
(define (count-white bd)
  (cdr (count-points bd)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (get-column bd c) ; returns the cth column as vector
  (define res (make-vector 8 'x))
  (define (helper r)
    (if (> r 7) res (begin (vector-set! res r (2d-vector-ref bd r c))
                           (helper (+ r 1))))
    )
  (helper 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (get-diagonal bd r c) ;returns the diagonal on which r c lies, isn't that cool :)
  (define vec (make-vector (- 8 (abs (- r c))) 'x))
  (define diff (abs (- r c)))
  
  (define (take-diagonal-ele r c vec) ; arguments are locations which are ends of the board like (0 3) , (4 0) ,(7 0)
    (if (or (> r 7) (> c 7)) vec (take-diagonal-ele (+ r 1) (+ c 1) (begin
                                                                      (vector-set! vec (min r c) (2d-vector-ref bd r c))
                                                                      vec))) 
    )
  
  (if (>= r c) (take-diagonal-ele diff 0 vec)
      (take-diagonal-ele 0 diff vec)
      )
  )

(define (get-other-diagonal bd r c)
  (define mirrored-bd (vector-map (lambda (x) (list->vector (reverse (vector->list x)))) bd))
  (get-diagonal mirrored-bd r (- 7 c))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (get-row bd r)
  (vector-ref bd r))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (place-coin r c coin)  ;self-explanatory
  (2d-vector-set! board r c coin)
  )

(define (place-coins l)  ;takes argument as list of list (format of list row col coin) and places coins at all those locations
  ; example of l (list '(3 3 0) '(3 4 1) '(4 3 1) '(4 4 0))
  (cond[(not (null? l)) (begin (let*([r (car (car l))]
                                     [c (cadr (car l))]
                                     [coin (caddr (car l))])
                                 (place-coin r c coin))
                               (place-coins (cdr l)))])
  )
;;
(define (initialise-board) ;produces  0 1  out of board which was earlier all false
  ;          1 0 
  (place-coins (list '(3 3 0) '(3 4 1) '(4 3 1) '(4 4 0))))
(initialise-board)
;;;
(define (full-board? bd) ;checks whether all the places contains coins or not if all the places contains coins board is filled
  ; and the game has ended,Then it returns #t
  (define (helper r c)
    (if (= r 8) #t (if (= c 7) (if (number? (2d-vector-ref bd r c)) (helper (+ r 1) 0) #f)
                       (if (number? (2d-vector-ref bd r c)) (helper r (+ c 1)) #f)))
    )
  (helper 0 0))
(define (end-game? bd)
  (or (full-board? bd) (and (null? (available-choices bd 1)) (null? (available-choices bd 0)))))
;;;;;
(define (occurences-of-coin bd coin) ;returns the list of co-ordinates of occurences of a coin (co-ordinates a cons r c)
  
  (define (helper r c res)
    (if (= r 8) res (if (equal? coin (2d-vector-ref bd r c)) (if (= c 7) (helper (+ r 1) 0 (cons (cons r c) res))
                                                                 (helper r (+ c 1) (cons (cons r c) res)))
                        (if (= c 7) (helper (+ r 1) 0 res)
                            (helper r (+ c 1) res))))
    )
  (helper 0 0 '()))
;;;;;;;;;;;;;;;;;;;;;;;;
(define (available-choices bd coin) ;returns the list of all the available choices for a player if no choice then returns '()
  ;
  (define occur-of-coin (occurences-of-coin bd (change coin)))
  (define (helper l res)
    (if (null? l) res (helper (cdr l) (append (available-choices-singular bd (car (car l)) (cdr (car l)) coin) res))))
  ;(remove-redundant
  ;(let*([res (check-last? bd)])
  ;   (if (pair? res) (list res) (helper occur-of-coin '()))))
  (remove-redundant (helper occur-of-coin '()))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (remove-redundant l)   ;;;; input '(1 2 3 3 4 5 5)----->>>>> output '(1 2 3 4 5)
  (define res '())
  (map (lambda (x) (if (ormap (lambda (y) (equal? y x)) res) 0 (begin (set! res (cons x res))
                                                                      0))) l)
  (reverse res)
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (list-contains? x l)  ;;;; input 1 '(3 4 2 1) ------>>>> output #t
  (ormap (lambda (y) (equal? x y)) l)  ;;;; input 5 '(3 4 2 1) ------>>>> output #f
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (available-choices-singular bd r c coin) ;returns the list of all the available choices corresponding to a coin at
  ; defined position
  (define choices (list (cons (- r 1) (- c 1)) (cons (- r 1) c) (cons (- r 1) (+ c 1))
                        (cons r (+ c  1)) (cons r (- c 1))
                        (cons (+ r 1) (- c 1)) (cons (+ r 1) c) (cons (+ r 1) (+ c 1))))
  (define proposed-choices (filter (lambda (x) (and (>= (car x) 0) (< (car x) 8) (>= (cdr x) 0) (< (cdr x) 8)
                                                    (not (number? (2d-vector-ref bd (car x) (cdr x))))
                                                    )) choices))
  
  (filter (lambda (x) (possible? bd (car x) (cdr x) r c)) proposed-choices)
  
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (possible? bd r1 c1 r c)
  (cond[(= r1 r) (check-possiblity c1 c (get-row bd r))];(check-possibility r1 c1 r c )]
       [(= c1 c) (check-possiblity r1 r (get-column bd c))]
       [(= (- r1 c1) (- r c)) (check-possiblity (min r1 c1)
                                                (min r c) (get-diagonal bd r c))]
       [else
        ( let*([sum (+ r1 c1)]
               [vec-length (if (> sum 7) (- 15 sum) (+ sum 1))])
           (check-possiblity (if (<= sum 7) r1 (let([reducing-factor (- 8 vec-length)])
                                                 (- r1 reducing-factor)
                                                 ))
                             (if (<= sum 7) r  (- r (- 8 vec-length)))
                             (get-other-diagonal bd r c))
           )]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (check-possiblity n1 n vec)
  (define coin-to-be-placed (vector-ref vec n1))
  (define pre-existing-coin (vector-ref vec n))
  (define len (vector-length vec))
  (define (helper x f) 
    (if (or (= x len) (< x 0)) #f
        (let*([coin (vector-ref vec x)])
          
          (if (number? coin) (if (= coin pre-existing-coin) (helper (f x) f)
                                 #t)
              #f))
        )
    ) 
  (if (> n n1) (helper (+ n 1) (lambda (x) (+ x 1)))
      (helper (- n1 1) (lambda (x) (- x 1))))
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (take-turn bd pos coin-to-be-placed) ;coin 1 (black) means player 1, coin 0 (white) means player 2
  (define (put-along r1 c1 r c) ;propogates the flip of coin
    (cond[(= c1 c) (put-along-col r1 r c)]
         [(= r1 r) (put-along-row c1 c r)]
         [(= (abs (- r1 c1)) (abs (- r c))) (put-along-diagonal r1 c1 r c)]
         [else (put-along-other-diagonal r1 c1 r c)]
         ))
  (define (put-along-col r1 r c) ;propogates the flip in col
    (cond[(and (<= r 7) (>= r 0) (not (equal? (2d-vector-ref bd r1 c) (2d-vector-ref bd r c)))
               (begin (2d-vector-set! bd r c coin-to-be-placed)
                      (if (> r r1) (put-along-col (+ r1 1) (+ r 1) c) (put-along-col (- r1 1) (- r 1) c))))]))
  
  (define (put-along-row c1 c r)
    (cond[(and (<= c 7) (>= c 0) (not (equal? (2d-vector-ref bd r c1) (2d-vector-ref bd r c))))
          (begin (2d-vector-set! bd r c coin-to-be-placed)
                 (if (> c c1) (put-along-row (+ c1 1) (+ c 1) r) (put-along-row (- c1 1) (- c 1) r)))
          ]))
  (define (put-along-diagonal r1 c1 r c)
    (cond[(and (>= r 0) (>= c 0) (<= r 7) (<= c 7) (not (equal? (2d-vector-ref bd r c) (2d-vector-ref bd r1 c1))))
          (begin (2d-vector-set! bd r c coin-to-be-placed)
                 (if (> r r1) (put-along-diagonal (+ r1 1) (+ c1 1) (+ 1 r) (+ 1 c))
                     (put-along-diagonal (- r1 1) (- c1 1) (- r 1) (- c 1))
                     ))]))
  (define (put-along-other-diagonal r1 c1 r c)
    (cond[(and (>= r 0) (>= c 0) (<= r 7) (<= c 7) (not (equal? (2d-vector-ref bd r c) (2d-vector-ref bd r1 c1))))
          (begin (2d-vector-set! bd r c coin-to-be-placed)
                 (if (> r r1) (put-along-other-diagonal (+ r1 1) (- c1 1) (+ r 1) (- c 1))
                     (put-along-other-diagonal (- r1 1) (+ c1 1) (- r 1) (+ c 1))
                     ))
          ]))
  (define (iter l)
    (cond[(not (null? l)) (begin (2d-vector-set! bd (car pos) (cdr pos) coin-to-be-placed)
                                 (put-along (car pos) (cdr pos) (car (car l)) (cdr (car l)))
                                 (iter (cdr l)))])
    )
  (if (list-contains? pos (available-choices bd coin-to-be-placed)) 
      (let*([r (car pos)]
            [c (cdr pos)]
            [other-coin (change coin-to-be-placed)])
        (let*([possible-expansions-along (list (cons (- r 1) (- c 1)) (cons (- r 1) c) (cons (- r 1) (+ c 1))
                                               (cons r (+ c  1)) (cons r (- c 1))
                                               (cons (+ r 1) (- c 1)) (cons (+ r 1) c) (cons (+ r 1) (+ c 1)))]
              [proposed-expansions-along (filter (lambda (x) (and (>= (car x) 0) (< (car x) 8) (>= (cdr x) 0) (< (cdr x) 8)
                                                                  (equal? (2d-vector-ref bd (car x) (cdr x)) other-coin))
                                                   ) possible-expansions-along)]
              [expansions-along (filter (lambda (x) (possible? bd r c (car x) (cdr x))) proposed-expansions-along)]
              )
          
          (iter expansions-along)
          ;(map (lambda (x) (begin (2d-vector-set! bd  r c coin-to-be-placed)
          ;                        (put-along r c (car x) (cdr x)))) expansions-along)
          
          )
        
        )  
      (error "Illegal Move"))  
  )
;-;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (take-turn-without-effect2orig bd pos player)
  ( let*([temp (create-temp bd)])
     (begin (take-turn temp pos player)
            temp)
     )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;[AI part];;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;[Computer Vs Human];;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define infinity 100)
(define neg-infinity -100)

(define (minimax bd max-depth player alpha beta)
  (define list-of-choices (available-choices bd player))
  (define best-move (initial-best list-of-choices))
  (define (dep1 l best-val)    ;function which sees operation on depth1
    
    (cond[(and (> beta alpha) (not (null? l)))
           (
            let*([new-board (take-turn-without-effect2orig bd (car l) player)]
                 [value (minimax-helper new-board 1 max-depth player
                                 player alpha beta)])
               (begin  
                     
                       (if (> value best-val) (set! best-move (car l)) (set! best-move best-move))
                       (set! best-val (max best-val value))
                       (set! alpha (max alpha best-val))
                       (dep1 (cdr l) best-val)
                       )
             )])
   )
 (begin (dep1 list-of-choices neg-infinity)
        best-move)
  )
(define (minimax-helper bd depth max-depth player orig-player alpha beta) ;(minimax-helper bd 1(ex) 3 1 1 alpha beta)
  (define list-of-choices (available-choices bd player))
  (define (iter-max l best-val) ;iterates over the choices when the player is maximizing
    (cond[(and (> beta alpha) (not (null? l)))
           (
            let*([new-board (take-turn-without-effect2orig bd (car l) player)]
                 [value (minimax-helper new-board (+ depth 1) max-depth (change player)
                                 player alpha beta)])
               (begin  
                      
                       (set! best-val (max best-val value))
                       (set! alpha (max alpha best-val))
                       (iter-max (cdr l) best-val)
                       )
             )]
         [else best-val])
    )
  (define (iter-min l best-val)  ;similar to iter-max ,only diff. is it looks for minimum
    (cond[(and (not (null? l)) (> beta alpha))
          (
           let*([new-board (take-turn-without-effect2orig bd (car l) player)]
                [value (minimax-helper new-board (+ depth 1) max-depth (change player)
                                 player alpha beta)])
            (begin 
                   (set! best-val (min best-val  value))
                   (set! beta (min best-val beta))
                   (iter-min (cdr l) best-val))
            )]
         [else best-val]))
  (cond[(= depth max-depth) (if (= orig-player 1) (count-black bd) (count-white bd))]
       [(= player orig-player) (iter-max list-of-choices neg-infinity)]
       [else (iter-min list-of-choices infinity)]
       )
  )
(define (initial-best l)
  (define corners '())
  (define edges '())
  (define (iter l1)
    (cond[(not (null? l1))
          (let*([x (car (car l1))]
                [y (cdr (car l1))])
            (if (or (and (= y 0) (= x 7)) (and (= y 7) (= x 7)) (and (= x 0) (= y 0))
                    (and (= x 0) (= y 7)))
                (begin (set! corners (cons (car l1) corners))
                       (iter (cdr l1)))
                (begin
                  (cond[(or (= x 0) (= y 0) (= x 7) (= y 7))
                        (set! edges (cons (car l1) edges))])
                  (iter (cdr l1)))
                )
            )]))
  (iter l)

  (if (null? corners) (if (null? edges) (car l) (car edges)) (car corners)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                              [G R A P H I C S]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;[first frame];;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define welcomeframe (new frame% [label "Welcome"];just for fun kinda looks like game loading
                          [width 1368]
                          [height 768]))

;the canvas on which initial image loading is visible
(define my-canvas(new canvas% [parent welcomeframe]
                      [paint-callback (lambda (canvas1 dc) (send dc draw-bitmap (make-object bitmap% "othello.jpg") -2 0))]))

(send welcomeframe fullscreen #t)

(send welcomeframe create-status-line)

(send welcomeframe set-status-text "@Racket corporation")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;2nd frame in which selection choices come ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define game-mode-frame (new frame%
                             [label "select-mode-of-game"]
                             [width 540]
                             [height 400]
                             [x 400]
                             [y 100]))
;panel of type of game
(define type-row
  (new horizontal-panel%
       [parent game-mode-frame]
       [min-height 108]))


;panel of level
(define level-row
  (new horizontal-panel%
       [parent game-mode-frame]
       [min-height 108]))

;start new game
(new button%
     [label "&Start game"]
     [parent game-mode-frame]
     [callback (lambda (button event) (begin (send game-mode-frame show #f)
                                             (if (equal? type 'vs-human) (send board-frame show #t)
                                                 (send board-frameAI show #t)
                                                      )))]);pending
;cancel
(new button%
     [label "&EXIT"]
     [parent game-mode-frame]
     [callback (lambda (button event) (send game-mode-frame show #f)
                                     )]);pending


(define typemessage (new message% [parent type-row] [label "TYPE"]))
(define levelmessage (new message% [parent level-row]    [label "-------------------"]))


;vs computer
(define type1-button (new button%
                          [parent       type-row]
                          [label        "VS &COMPUTER"]
                          [callback     (lambda (button event) (begin (send levelmessage set-label "DIFFICULTY LEVEL")
                                                                      (level1-button)
                                                                      (level2-button)
                                                                      (level3-button)
                                                                      (set! type 'vs-computer)))]))
;vs human
(define type2-button (new button%
                          [parent       type-row]
                          [label        "VS &HUMAN"]
                          [callback     (lambda (button event) (set! type 'vs-human))]))


;easy
(define (level1-button) (new button%
                           [parent       level-row]
                           [label        "&EASY"]
                           [callback     (lambda (button event) (set! level 1))]))
;medium
(define (level2-button) (new button%
                           [parent       level-row]
                           [label        "&MEDIUM"]
                           [callback     (lambda (button event) (set! level 2))]))
;hard
(define (level3-button) (new button%
                           [parent       level-row]
                           [label        "&HARD"]
                           [callback     (lambda (button event) (set! level 4))]))

;4th frame in which you play the game;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(define board-frame (new frame% [label "game is on!"]
                         [width 538]
                         [height 595]
                         [x 400]
                         [y 100]))
(define board-frameAI (new frame% [label "game is on!"]
                         [width 538]
                         [height 595]
                         [x 400]
                         [y 100]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;[ending frames of losing or winning]

(define winframe (new frame% [label "GAME_OVER"];just for fun kinda looks like game loading
                          [width 700]
                          [height 700]))
(define winframe-canvas (new canvas% [parent winframe]
                        [paint-callback (lambda (canvas1 dc) (send dc draw-bitmap (make-object bitmap% "youwin.jpeg") -2 0))]))

(define winframe-H (new frame% [label "GAME_OVER"];just for fun kinda looks like game loading
                          [width 700]
                          [height 700]))
(define winframeH-canvas (new canvas% [parent winframe-H]
                        [paint-callback (lambda (canvas1 dc) (send dc draw-bitmap (make-object bitmap% "black-wins.jpeg") -2 0))]))

(define looseframe (new frame% [label "GAME_OVER"];just for fun kinda looks like game loading
                          [width 736]
                          [height 407]))
(define looseframe-canvas (new canvas% [parent looseframe]
                        [paint-callback (lambda (canvas1 dc) (send dc draw-bitmap (make-object bitmap% "lose.jpeg") -2 0))]))
(define looseframe-H (new frame% [label "GAME_OVER"];just for fun kinda looks like game loading
                          [width 700]
                          [height 700]))
(define loooseframeH-canvas (new canvas% [parent looseframe-H]
                         [paint-callback (lambda (canvas1 dc) (send dc draw-bitmap (make-object bitmap% "white-wins.jpeg") -2 0))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (black-points frame) (new text-field% [label "BLACK STONE"]
                                      [init-value "2"]
                                      [parent frame]))
(define (white-points frame) (new text-field% [label "WHITE STONE"]
                                      [init-value "2"]
                                      [parent frame]))
(define black-scoreH (black-points board-frame))
(define black-scoreAI (black-points board-frameAI))
(define white-scoreAI (white-points board-frameAI))
(define white-scoreH (white-points board-frame))
(define (update-points P)
  (let*([b (car P)]
        [w (cdr P)])
    (if (equal? type 'vs-human) (begin (send black-scoreH set-value (number->string b))
                                       (send white-scoreH set-value (number->string w)))
        (begin (send black-scoreAI set-value (number->string b))
               (send white-scoreAI set-value (number->string w))))))
    


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;finds changed pos in the board
(define (find-changed-pos cboard pboard)
  (define lpos `())
  (define i 0)
  (define j 0)
  (for (set! i 0) : (< i 8) : (set! i (+ i 1)) :
    (for (set! j 0) : (< j 8) : (set! j (+ j 1)) :
      (let*([cb (2d-vector-ref cboard i j)]
            [pb (2d-vector-ref pboard i j)])
        (cond [(not (equal? cb pb)) (set! lpos (cons (cons (cons i j) cb) lpos))]))))
  lpos)
;finds the pos by knowing x and y cordinates

(define (find-my-pos x y)
  (cons (inexact->exact (floor (/ (- y 34) 62.25))) (inexact->exact (floor (/ (- x 19.5) 63.0))) ))

;finds the cordinates by knowing pos

(define (find-my-cordinates pos)
  (let*([i (* 63.0 (cdr pos))]
        [j (* 62.25 (car pos))])
    (cons (+ i 20.0) (+ j 34))))

;draw stones on canvas;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (draw-stones-on-canvas dc lpos);lpos is of type (cons (cons i j) state)
  (define black (make-object bitmap% "black.jpeg"))
  (define white (make-object bitmap% "white.jpeg"))
  (define (draw-helper l)
    (cond [(not (null? l)) (let*([cord (find-my-cordinates (caar l))]
                                 [x (car cord)]
                                 [y (cdr cord)]
                                 [color (if (equal? (cdar l) 1) black white)])
                             (begin (send dc draw-bitmap color x y)
                                    (draw-helper (cdr l))))]))
  (draw-helper lpos))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;hof

(define (draw-helper l dc image)
    (cond [(not (null? l)) (let*([cord (find-my-cordinates (car l))]
                                 [x (car cord)]
                                 [y (cdr cord)])
                             (begin (send dc draw-bitmap image x y)
                                    (draw-helper (cdr l) dc image)))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;draw available palces to put a coin
(define (draw-avail-places-on-canvas dc apos cstate);apos is of type (list (cons i j) ...)
  (define ablack (make-object bitmap% "place-black.jpeg"))
  (define awhite (make-object bitmap% "place-white.jpeg"))
  (define color (if (equal? cstate 1) ablack awhite))
  (draw-helper apos dc color))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;erases previous places
(define (erase-previous-places dc)
  (define empty (make-object bitmap% "empty.jpeg"))
  (draw-helper available-places dc empty))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define AIframecanvas ;;this is a  object that is inherited from canvas%
  ;;but has one function changed
  (class canvas%
    (define (computer-turn board)
      (let*([saved-board (2d-vector-copy board)]
            [comp-move (minimax board level state neg-infinity infinity)];ofcorse state is of computer
            [comp-changed-pos (begin (take-turn board comp-move state);state is correspondence with avail-places list
                                     (update-points (count-points board))
                                     (find-changed-pos board saved-board))])
        (begin (draw-stones-on-canvas (send this get-dc) comp-changed-pos)
               (set! state (change state))
               (set! available-places (available-choices board state))
               (cond [(end-game? board) (if (> (count-white board) (count-black board))
                                                           (send looseframe show #t)
                                                           (send winframe show #t))]
                     [(null? available-places) (begin (set! state (change state));;;more to add[-------]
                                                      (set! available-places (available-choices board state))
                                                      (computer-turn board))]
                     [else (draw-avail-places-on-canvas (send this get-dc) available-places state)]))))
    (define/override (on-event mouse-event);for vs computer (for now computer is 0)
      (cond [(eq? (send mouse-event get-event-type) 'left-down) ; left-down == normal mouse click
             (let*([x (send mouse-event get-x)]
                   [y (send mouse-event get-y)]
                   [pos (find-my-pos x y)]
                   );find-my-cord.. works according to range
               (cond[(is-present? pos available-places)
                     (let*([saved-board (2d-vector-copy board)]
                           [changed-pos (begin (take-turn board pos state);state is correspondence with avail-places list
                                               (update-points (count-points board))
                                               (find-changed-pos board saved-board))]
                           );will return list of (cons pos state)
                       (begin (erase-previous-places (send this get-dc))
                              (draw-stones-on-canvas (send this get-dc) changed-pos)
                              (set! state (change state))
                              (set! available-places (available-choices board state))
                              (cond [(end-game? board) (if (> (count-white board) (count-black board))
                                                           (send looseframe show #t)
                                                           (send winframe show #t))
                                                              ]
                                    [(null? available-places) (begin (set! state (change state));;;more to add[-------]
                                                                     (set! available-places (available-choices board state))
                                                                     (draw-avail-places-on-canvas (send this get-dc) available-places state))]
                                    [else (computer-turn board)])))]))]))
      
    (super-new [parent board-frameAI])))

(define AIboard-canvas (new AIframecanvas;-----------------[here the game runs!]
                          [paint-callback ;;like a callback function this keeps executing, but it kind of "paints" on
                           ; the canvas with some bitmap
                           (lambda (canvas1 dc)    
                             (send dc draw-bitmap
                                   (make-object bitmap% "board.jpeg") ; all png images need to be stored as a bitmap (sort of an array of 0s and 1s)
                                   0 0)
                             )]))
(define HVSHframe-canvas ;;this is a function that returns an object that is inherited from canvas%
  ;;but has one function changed
  (class canvas%
    (define/override (on-event mouse-event);only for HVSH play at present
      (cond [(eq? (send mouse-event get-event-type) 'left-down) ; left-down == normal mouse click
             (let*([x (send mouse-event get-x)]
                   [y (send mouse-event get-y)]
                   [pos (find-my-pos x y)]
                   );find-my-cord.. works according to range
               (cond[(is-present? pos available-places)
                     (let*([saved-board (2d-vector-copy board)]
                           [changed-pos (begin (take-turn board pos state);state is correspondence with avail-places list
                                               (update-points (count-points board))
                                          (find-changed-pos board saved-board))]
                           );will return list of (cons pos state)
                       (begin (erase-previous-places (send this get-dc))
                              (draw-stones-on-canvas (send this get-dc) changed-pos)
                              (set! state (change state))
                              (set! available-places (available-choices board state))
                              (cond [(end-game? board) (if (> (count-white board) (count-black board))
                                                           (send looseframe-H show #t)
                                                           (send winframe-H show #t))
                                                           ]
                                    [(null? available-places) (begin (set! state (change state));;;more to add[-------]
                                                                     (set! available-places (available-choices board state))
                                                                     (draw-avail-places-on-canvas (send this get-dc) available-places state))]
                                    [else (draw-avail-places-on-canvas (send this get-dc) available-places state)])))]))])
             )
  (super-new [parent board-frame])))

(define HVSHboard-canvas (new HVSHframe-canvas;-----------------[here the game runs!]
                         [paint-callback ;;like a callback function this keeps executing, but it kind of "paints" on
                      ; the canvas with some bitmap
                      (lambda (canvas1 dc)    
                        (send dc draw-bitmap
        (make-object bitmap% "board.jpeg") ; all png images need to be stored as a bitmap (sort of an array of 0s and 1s)
        0 0)
        )]))













(send welcomeframe show #t)
(sleep/yield 2)
(send welcomeframe show #f)
(send game-mode-frame show #t)
;useful global variables)
(define type 'undefined)
(define level 'undefined)
;related to game
(define state 1);1 is black 
(define available-places (available-choices board state))

