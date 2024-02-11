#lang racket
(require racket/gui)

(define edges
  (hash
    'a '((b d l) (2 4 4) (0.5 1 1))
    'b '((a c d e) (2 3 7 10) (0.5 1 2.5 3))
    'c '((b i j) (3 5 1) (1 1.5 0.5))
    'd '((a b k f) (4 7 10 2) (1 2.5 3 0.5))
    'e '((b f i g) (10 7 8 9) (3 2.5 2.5 3))
    'f '((d e g k n) (2 7 1 2 3) (0.5 2.5 0.5 0.5 1))
    'g '((e f h) (9 1 6) (3 0.5 2))
    'h '((g i) (6 5) (2 1.5))
    'i '((e h c j o) (8 5 5 6 6) (2.5 1.5 1.5 2 2))
    'j '((c i) (1 6) (0.5 2))
    'k '((d f m) (10 2 7) (3 0.5 2.5))
    'm '((k) (7) (2.5))
    'n '((f) (3) (1))
    'l '((a) (4) (1))
    'o '((i) (6) (2))))

(define edge
  (hash
    'a '(b d l)
    'b '(a c d e) 
    'c '(b i j) 
    'd '(a b k f)
    'e '(b f i g)
    'f '(d e g k n)
    'g '(e f h)
    'h '(g i) 
    'i '(e h c j o)
    'j '(c i)
    'k '(d f m) 
    'm '(k)
    'n '(f)
    'l '(a) 
    'o '(i)))


(define nodes '(a b c d e f g h i j k l m n o))



   

(define (remove lst)
  (cond
    ((null? lst) '())
    ((not (pair? (car lst))) lst)
    (else (remove(car lst)))))

(define (mixfinder graph start end)
  (define visited (make-hash))
  
  (define (mixfinderG current path)
    (if (eq? current end)
        (list (reverse (cons current path)))
        (begin
          (hash-set! visited current #t)
          (let loop ((neighbors (hash-ref graph current '()))
                     (result '())
                     (count 3))
            (cond
              ((null? neighbors) result)
              ((= count 0) result)
              (else
               (let ((neighbor (car neighbors)))
                 (unless (hash-ref visited neighbor #f)
                   (let ((new-path (mixfinderG neighbor (cons current path))))
                     (unless (null? new-path)
                       (set! result (append result (list new-path)))
                       (set! count (- count 1))))))
                 (loop (cdr neighbors) result count)))))))
  
  (mixfinderG start '()))









(define mainframe
  (new frame%
       [label "Route Finder"]
       [width 280]
       [height 500]
       ))
(define mainpanel
  (new horizontal-panel%
       [parent mainframe]
       ))


(define menubar (new menu-bar%
                     (parent mainframe)))
(define menu1 (new menu%
                   [label "&Options"]
                   [parent menubar]))
(define mapmenu 
  (new menu-item%
       [parent menu1]
       [label "Map"]
       [callback (lambda (menu-event item)
                   (define map-window (new frame% [label "Map Window"] [width 500] [height 500]))
                   (define (draw-map canvas dc)
                     (send dc set-font (make-object font% 20 'default 'normal 'normal))
                     (send dc draw-text "Thank you" 50 50))
                   (define canvas (new canvas% [parent map-window] [paint-callback (lambda (canvas dc) (draw-map canvas dc))])) ; Modified to pass canvas
                   (send map-window show #t))]))

(define exitmenu (new menu-item%
                     [parent menu1]
                     [label "Exit"]
                     [callback (lambda (menu-event item)
                                              (send mainframe show #f))]))

(define vertipanel
  (new vertical-panel%
       [parent mainpanel]
       [border 5]
       [spacing 13]
       [alignment '(center top)]
       [min-width 100]
       [stretchable-width 100]))

(define input1 (new list-box%
                 [parent vertipanel]
                 [choices '("A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O")]
                 [label "Start:            "]))



(define input2 (new list-box%
                 [parent vertipanel]
                 [choices '("A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O")]
                 [label "Destination: "]))

 

(define (list-to-string1 lst)
  (cond
    ((null? lst) "")
    ((list? (car lst)) (list-to-string1 (car lst)))
    (else (format "~a ~a" (car lst) (list-to-string1 (cdr lst))))))

(define (list-to-string2 data)
  (string-trim
   (list-to-string1 (flatten (reverse data)))))

(define (split-string str)
  (let loop ((chars (string->list str))  
             (result '())                
             (current '()))              
    (cond
      ((null? chars)                  
       (reverse (cons (list->string (reverse current)) result)))
      ((char=? (car chars) (string-ref str 0)) 
       (loop (cdr chars) (cons (list->string (reverse current)) result) (list (car chars))))
      (else                           
       (loop (cdr chars) result (cons (car chars) current))))))

(define (remove-empty lst)
  (filter (lambda (x) (not (zero? (string-length x)))) lst))


(define radioitems '("Duration" "Price"))
(define radiolist (new radio-box%
                       [parent vertipanel]
                       [choices radioitems]
                       [label #f]))
(define (show-output output)
  (define dialog (new dialog% [label "Route Display"] [width 200] [height 150]))
  (define msg (new message% [parent dialog] [label output]))
  (send dialog show #t))




(define button1
  (new button%
       [parent vertipanel]
       [label "Find Path"]
       [callback (lambda (button evt) (show-output (remove-empty(split-string(list-to-string2(mixfinder edge (list-ref nodes (send input1 get-selection)) (list-ref nodes (send input2 get-selection))))))))]
       
       ))

(send mainframe show #t)
