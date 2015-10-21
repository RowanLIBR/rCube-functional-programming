;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functional Programming Assignment --- Fixing The World    ;;
;; 25/3/15                                                   ;;
;; <Add your name and student number here>   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;-------------------HELPER FUNCTIONS----------------------
;; ;print function for debugging purposes
#lang racket
(define (print . args)
  (cond ((not (null? args))
        (display (car args))
        (apply print (cdr args)))
  )
)

;; ;gets nth index of 0-indexed list. Can use list-ref instead
(define (index lst idx)
    (if (null? lst)
        lst
        (if (= idx 0)
            (car lst)
            (index (cdr lst) (- idx 1))
        )
    )
)
;; ;TESTS
 ;; (print (= 1 (index '(1 2 3 4 5) 0)) "\n")
; ; (print (= 4 (index '(1 2 3 4 5) 3)) "\n")
;; ; (print (not (= 1 (index '(1 2 3 4 5) 2))) "\n")
;; ; (print (not (= 0 (index '(1 2 3 4 5) 0))) "\n")

;; ;checks if an item is in a list
;; You might want to do a more efficient version of this.
;;
(define (in item lst)
    (if (null? lst)
        #f
        (if (equal? item (car lst))
            #t
            (in item (cdr lst))
        )
    )
)
;; ;TESTS
;; ; (print (in 1 '(1 2 3)) "\n")
;; ; (print (in 2 '(1 2 3)) "\n")
;; ; (print (not (in 4 '(1 2 3))) "\n")
;; ; (print (in '(1 2) '((1 2) (3 4) 5)) "\n")

;; ;helper function for finding the length of a list
(define (lengthHelper n lst)
    (if (null? lst)
        n
        (lengthHelper (+ n 1) (cdr lst))
    )
)

;; ;finds length of a list
(define (length lst)
    (lengthHelper 0 lst)
)
;; ;TESTS
;; ; (print (= 4 (length '(1 2 3 4))) "\n")
;; ; (print (= 1 (length '(1))) "\n")
;; ; (print (= 2 (length '((1 2) (3 4)))) "\n")
;; ; (print (not (= 4 (length '(1 2 3 4 5)))) "\n")
;; ;-----------------------------------------------------------


;---------------------SOLVED STATES------------------------
;solved states of a 2x2x2 rubiks cube
(define solvedStates
    '(  ((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))
        ((3 1) (1 1) (4 1) (2 1) (7 3) (5 3) (8 3) (6 3))
        ((4 1) (3 1) (2 1) (1 1) (8 3) (7 3) (6 3) (5 3))
        ((2 1) (4 1) (1 1) (3 1) (6 3) (8 3) (5 3) (7 3))

        ((5 5) (1 6) (7 5) (3 6) (6 5) (2 6) (8 5) (4 6))
        ((7 5) (3 6) (8 5) (4 6) (5 5) (1 6) (6 5) (2 6))
        ((8 5) (4 6) (6 5) (2 6) (7 5) (3 6) (5 5) (1 6))
        ((6 5) (2 6) (5 5) (1 6) (8 5) (4 6) (7 5) (3 6))

        ((2 5) (6 6) (4 5) (8 6) (1 5) (5 6) (3 5) (7 6))
        ((4 5) (8 6) (3 5) (7 6) (2 5) (6 6) (1 5) (5 6))
        ((3 5) (7 6) (1 5) (5 6) (4 5) (8 6) (2 5) (6 6))
        ((1 5) (5 6) (2 5) (6 6) (3 5) (7 6) (4 5) (8 6))

        ((7 1) (8 1) (5 1) (6 1) (3 3) (4 3) (1 3) (2 3))
        ((5 1) (7 1) (6 1) (8 1) (1 3) (3 3) (2 3) (4 3))
        ((6 1) (5 1) (8 1) (7 1) (2 3) (1 3) (4 3) (3 3))
        ((8 1) (6 1) (7 1) (5 1) (4 3) (2 3) (3 3) (1 3))

        ((3 2) (4 2) (7 4) (8 4) (1 2) (2 2) (5 4) (6 4))
        ((1 2) (3 2) (5 4) (7 4) (2 2) (4 2) (6 4) (8 4))
        ((2 2) (1 2) (6 4) (5 4) (4 2) (3 2) (8 4) (7 4))
        ((4 2) (2 2) (8 4) (6 4) (3 2) (1 2) (7 4) (5 4))

        ((5 2) (6 2) (1 4) (2 4) (7 2) (8 2) (3 4) (4 4))
        ((7 2) (5 2) (3 4) (1 4) (8 2) (6 2) (4 4) (2 4))
        ((8 2) (7 2) (4 4) (3 4) (6 2) (5 2) (2 4) (1 4))
        ((6 2) (8 2) (2 4) (4 4) (5 2) (7 2) (1 4) (3 4))
    )
)
;; ;-----------------------------------------------------


;; ;---------------------QUESTION 1.1-----------------------
;; ;helper function for rotating the cube. Recalculates the various orientations
;; ;of the sub-cubes
(define (recalculateOrientation orientation axis)
    (cond
        [(= axis 0);x
            (if (> orientation 4)
                orientation
                (if(= orientation 4)
                    1
                    (+ orientation 1)
                )
            )
        ]
        [(= axis 1)
            (if (or (= orientation 1) (= orientation 3))
                orientation
                (cond
                    [(= orientation 2) 6]
                    [(= orientation 4) 5]
                    [(= orientation 5) 2]
                    [(= orientation 6) 4]
                )
            )
        ]
        [(= axis 2)
            (if (or (= orientation 2) (= orientation 4))
                orientation
                (cond
                    [(= orientation 1) 5]
                    [(= orientation 3) 6]
                    [(= orientation 5) 1]
                    [(= orientation 6) 3]
                )
            )
        ]
         
        [(= axis 3);X
            (if (> orientation 4)
                orientation
                (if(= orientation 1)
                    4
                    (- orientation 1)
                )
            )
        ]
        [(= axis 4)
            (if (or (= orientation 2) (= orientation 4))
                orientation
                (cond
                    [(= orientation 5) 1]
                    [(= orientation 6) 3]
                    [(= orientation 1) 5]
                    [(= orientation 3) 6]
                )
            )
        ] 
       
    )
)
;; ;TESTS
 ;(print (= 2 (recalculateOrientation 1 0)) "\n")
;(print (= 5 (recalculateOrientation 5 0)) "\n")
 ;(print (= 1 (recalculateOrientation 1 1)) "\n")
; (print (= 6 (recalculateOrientation 2 1)) "\n")
 ;(print (= 5 (recalculateOrientation 1 2)) "\n")
 ;(print (= 2 (recalculateOrientation 2 2)) "\n")

;rotations are performed using the left hand rule
;rotates left 4 cubes along x axis
(define (rotateX ispositive state)
	;(list '((5 4) (2 1) (1 2) (4 1) (7 4) (6 3) (3 2) (8 3)) (list "x")) ;;; *TODO* ;;;
        (if (eq? ispositive #t)
         (list (list(list (car(list-ref state 4)) (recalculateOrientation (car(cdr (list-ref state 4)))0)) ; hardcoding the change of id places. For those that change, recalc orientation
               (list-ref state 1)
               (list (car(list-ref state 0)) (recalculateOrientation (car(cdr (list-ref state 0)))0))
               (list-ref state 3)
               (list (car(list-ref state 6)) (recalculateOrientation (car(cdr (list-ref state 6)))0))
               (list-ref state 5)
               (list (car(list-ref state 2)) (recalculateOrientation (car(cdr (list-ref state 2)))0))
               (list-ref state 7))
               (list "x")
               
               
         )
         
         (list (list(list (car(list-ref state 2)) (recalculateOrientation (car(cdr (list-ref state 2)))3))
               (list-ref state 1)
               (list (car(list-ref state 6)) (recalculateOrientation (car(cdr (list-ref state 6)))3))
               (list-ref state 3)
               (list (car(list-ref state 0)) (recalculateOrientation (car(cdr (list-ref state 0)))3))
               (list-ref state 5)
               (list (car(list-ref state 4)) (recalculateOrientation (car(cdr (list-ref state 4)))3))
               (list-ref state 7))
               (list "X")
               
         )
         
  
  
         
)
)
;rotates bottom 4 cubes along y axis
(define (rotateY ispositive state)  ; hardcoding the change of id places. For those that change, recalc orientation
             
   ;	(list '((5 4) (2 1) (1 2) (4 1) (7 4) (6 3) (3 2) (8 3)) (list "y")) ;;; *TODO* ;;;
   (if (eq? ispositive #f)
         (list 
               (list(list-ref state 0)
               (list-ref state 1)
               (list-ref state 2)
               (list-ref state 3)
               (list (car(list-ref state 6)) (recalculateOrientation (car(cdr (list-ref state 6)))1))
               (list (car(list-ref state 4)) (recalculateOrientation (car(cdr (list-ref state 4)))1))
               (list (car(list-ref state 7)) (recalculateOrientation (car(cdr (list-ref state 7)))1))
               (list (car(list-ref state 5)) (recalculateOrientation (car(cdr (list-ref state 5)))1)))
               (list "Y")
               
               
         )
         
         (list 
               (list(list-ref state 0)
               (list-ref state 1)
               (list-ref state 2)
               (list-ref state 3)
               (list (car(list-ref state 5)) (recalculateOrientation (car(cdr (list-ref state 5)))1))
               (list (car(list-ref state 7)) (recalculateOrientation (car(cdr (list-ref state 7)))1))
               (list (car(list-ref state 4)) (recalculateOrientation (car(cdr (list-ref state 4)))1))
               (list (car(list-ref state 6)) (recalculateOrientation (car(cdr (list-ref state 6)))1)))
               (list "y")
               
         )
        
  
 ))

;rotates back 4 cubes along z axis
(define (rotateZ ispositive state)  ; hardcoding the change of id places. For those that change, recalc orientation
               (list-ref state 1)
;	(list '((5 4) (2 1) (1 2) (4 1) (7 4) (6 3) (3 2) (8 3)) (list "z")) ;;; *TODO* ;;;
  (if (eq? ispositive #t)
         (list 
               
               (list(list (car(list-ref state 1)) (recalculateOrientation (car(cdr (list-ref state 1)))2))
               (list (car(list-ref state 5)) (recalculateOrientation (car(cdr (list-ref state 5)))2))
               (list-ref state 2)
               (list-ref state 3)
               (list (car(list-ref state 0)) (recalculateOrientation (car(cdr (list-ref state 0)))2))
               (list (car(list-ref state 4)) (recalculateOrientation (car(cdr (list-ref state 4)))2))
               (list-ref state 6)
               (list-ref state 7))
               (list "z")
               
         )
         
   (list 
               
               (list(list (car(list-ref state 4)) (recalculateOrientation (car(cdr (list-ref state 4)))4))
               (list (car(list-ref state 0)) (recalculateOrientation (car(cdr (list-ref state 0)))4))
               (list-ref state 2)
               (list-ref state 3)
               (list (car(list-ref state 5)) (recalculateOrientation (car(cdr (list-ref state 5)))4))
               (list (car(list-ref state 1)) (recalculateOrientation (car(cdr (list-ref state 1)))4))
               (list-ref state 6)
               (list-ref state 7))
               (list "Z")
               
               
         )
  
        
  
)
)
;; ;helper for rotate function
(define (rotateHelper char state)
    (cond
        [(char=? char #\x) (car (rotateX #t state))]
        [(char=? char #\X) (car (rotateX #f state))]
        [(char=? char #\y) (car (rotateY #t state))]
        [(char=? char #\Y) (car (rotateY #f state))]
        [(char=? char #\z) (car (rotateZ #t state))]
        [(char=? char #\Z) (car (rotateZ #f state))]
    )
)

;; ;parses a string for rotations
(define (rotate rotations state)
    (if (= (string-length rotations) 0)
        state
        (rotate (substring rotations 1 (string-length rotations)) (rotateHelper (string-ref rotations 0) state))
    )
)

 (print (equal? (rotate "x" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) (car (rotateX #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))) "\n")
 (print (equal? (rotate "xyz" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) (car (rotateZ #t (car (rotateY #t (car (rotateX #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))))))) "\n")
 (print (equal? (rotate "xXx" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) (car (rotateX #t (car (rotateX #f (car (rotateX #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))))))) "\n")
 (print (not (equal? (rotate "xXy" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) (car (rotateX #f (car (rotateZ #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3)))))))) "\n")

(print(rotate "XY" '((2 5) (8 6) (1 2) (4 1) (5 4) (6 6) (7 5) (3 6))) "\n")

;-----------------------QUESTION 1.2-----------------------
;generates the successor states of the current given rubiks cube state
(define (generateSuccessorStates state prevMoves) 
    ;;(list
       ;; (list
           ;; '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))
          ;;  '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))
          ;;  '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))
          ;;  '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))
          ;;  '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))
          ;;  '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))
       ;; )
       ;; '(("x") ("X") ("y") ("Y") ("z") ("Z"))
     ;;; *TODO* ;;; 
  
  (list
       (list (car(rotateX #t state)) ; I want to leave out the moves attached at the end of the list so I use car
             (car(rotateX #f state)) ; all these steps generate all the possible states that can be reached in one move from an initial state
             (car(rotateY #t state))
             (car(rotateY #f state))
             (car(rotateZ #t state))
             (car(rotateZ #f state))
        )
       (list
       (append prevMoves (list "x")) ; keep a history of moves
       (append prevMoves (list "X"))
       (append prevMoves (list "y")) 
       (append prevMoves (list "Y"))
       (append prevMoves (list "z"))
       (append prevMoves (list "Z"))
     
       )
            )
  

  )


(print (equal? (generateSuccessorStates '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3)) '())
        (list
            (list
                 (car (rotateX #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))
                 (car (rotateX #f '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))
                 (car (rotateY #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))
                 (car (rotateY #f '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))
                 (car (rotateZ #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))
                 (car (rotateZ #f '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))
             )
             '(("x") ("X") ("y") ("Y") ("z") ("Z"))
         )
     )
 "\n")


;-----------------------------QUESTION 2.1--------------------------

;finds all the states at a specific depth
(define (genStates n state moves)
  (list (map car (genStatesHelper n state moves)) (map cadr (genStatesHelper n state moves))) ; producing the correct format, with states and moves grouped together
      
  )    
       
  ; )
    
  
  (define (genStatesHelper n state moves) ; I used two functions so that I could format the output correctly. Another option would be to make use of let
    (if (eq? n 0)
      (list (list state moves))
      (append
      (genStatesHelper (- n 1) (car (rotateX #t state)) (append moves (list "x")))
      (genStatesHelper (- n 1) (car (rotateX #f state)) (append moves (list "X")))
      (genStatesHelper (- n 1) (car (rotateY #t state)) (append moves (list "y")))
      (genStatesHelper (- n 1) (car (rotateY #f state)) (append moves (list "Y")))
      (genStatesHelper (- n 1) (car (rotateZ #t state)) (append moves (list "z")))
      (genStatesHelper (- n 1) (car (rotateZ #f state)) (append moves (list "Z")))
      )
    )
   )
    
    
            
   ; ((((5 4) (2 1) (1 2) (4 1) (7 4) (6 3) (3 2) (8 3)) ((3 4) (2 1) (7 2) (4 1) (1 4) (6 3) (5 2) (8 3)) ((1 1) (2 1) (3 1) (4 1) (6 3) (8 3) (5 3) (7 3)) ((1 1) (2 1) (3 1) (4 1) (7 3) (5 3) (8 3) (6 3)) ((2 5) (6 6) (3 1) (4 1) (1 5) (5 6) (7 3) (8 3)) ((5 5) (1 6) (3 1) (4 1) (6 5) (2 6) (7 3) (8 3))) ((x) (X) (y) (Y) (z) (Z))) ;;; *TODO* ;;;
;)
;----------------------------------------------------------


;---------------------------QUESTION 3.1-----------------------
;Solves a rubiks cube using breadth first search. Can solve up to roughly 7 moves.
(define (solveCube solved initial n)
  
  
    (Search (newBreadth  initial (+ 1 n)) initial (+ 1 n))
    ;'("Z", "Y", "X") ;;; *TODO* ;;;
    
  
)

  
 ; searches if any (produced successor) states correspond to a solved state 
(define (Search current initial n) 
  (if(eq? (in #t (map checkEqual current)) #t) ; use map to check for each sucessor state
    
    (cdr(list-ref current (indexOf (map checkEqual current) #t))) ; find index of #t in list produced by mapping. This index will correspond to the index of the sate which matches a solved state
    
   
     (solveCube solvedStates initial n)
  )
  )
  
; uses in fuction to check if a state provided is equal to a solved state
(define (checkEqual state)
  (if(eq? (in (car state) solvedStates) #t)
     
     #t
     #f
     )
  )
; generates successor states to search through
(define (newBreadth initial n)
   
   (genStatesHelper n initial '())
  )
; generates the index of a specific item in a list
(define (indexOf lst ele)
  (let loop ((lst lst)
             (idx 0))
    (cond ((empty? lst) #f)
          ((equal? (first lst) ele) idx)
          (else (loop (rest lst) (+ idx 1))))))

  
;---------------------------------------------------------------------
;TESTS
 ;(print (equal? '("Z" "Y" "X") (solveCube solvedStates (rotate "xyz" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) 0)) "\n")
 ;(print (equal? '((X)) (solveCube solvedStates (rotate "x" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) 0)) "\n")
(print(solveCube  solvedStates  (rotate "xy" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) 0))
;---------------------------------------------------------------------
