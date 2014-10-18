#lang racket
(require 2htdp/universe)
(require 2htdp/image)



(define (expand-by-rules symbols rules)
  (let loop ([symbols (reverse symbols)]
             [new-symbols (list)]
            )
      (cond [(null? symbols) new-symbols]
            [(assoc (first symbols) rules)
               => ;; Omituinen syntaksi cond-clausesta, ei selitetty vielä!
               (lambda (rule-found)
                   (loop (rest symbols) (append (rest rule-found) new-symbols))
               )
            ]
            [else (loop (rest symbols) (cons (first symbols) new-symbols))]
      )
  )
)

;; 
;; 

;; 
;; 
;; ⍕ ⍎

;; Other alternatives: ↖ & ↗, ↓ & ↑, ⏬ & ⏫, ⍒ & ⍋, ⍢ & ⍙, ⍥ & ⍜, ⍕ & ⍎, ⏁ & ⏂, ⍚  ⎏ & ⎐, ⚺ & ⚻, 入 & 出, ⛉ & ☖

(define rules-for-simple-fractal-tree '((0 1 PUSH ↰ 0 POP ↱ 0) (1 1 1)))

(define rules-for-simple-fractal-tree2 '((0 1 PUSH ↰ 0 POP ↱ PUSH 0 POP))) ;; This one keeps edge-lengths constant


;;  rules  : (X → F-[[X]+X]+F[+FX]-X), (F → FF)
;;  angle  : 25°
    

(define rules-for-fractal-plant '((X 1 ↰ PUSH PUSH X POP ↱ X POP ↱ 1 PUSH  ↱ 1 X POP ↰ X)
                                  (1 1 1)
                                 )
)

;; (animate (get-growing-tree-callback '(0) (lambda (syms) (expand-by-rules syms rules-for-simple-fractal-tree)) 28)

(define (test-it ticks)
  (animate (get-growing-tree-callback '(0) (lambda (syms) (expand-by-rules syms rules-for-simple-fractal-tree2)) ticks))
)
  
(define (get-growing-tree-callback seed apply-rules-fun every-nth-tick)
  (let ((our-private-symbols seed))
    (lambda (frametime)
         (cond [(zero? (modulo frametime every-nth-tick)) (set! our-private-symbols (apply-rules-fun our-private-symbols))])
         (turtle-for-tree our-private-symbols)
    )
  )
)

(define-struct paikka (x y))

  


(define (asteet-radiaaneiksi astetta) (* (/ pi 180) astetta))

;; Try (plant25 6)
;; and compare to http://en.wikipedia.org/wiki/L-system#Example_7:_Fractal_plant

(define (plant25 n)
  (let loop ((n n) (syms '(X)))
     (if (zero? n)
         (turtle-for-plant syms)
         (loop (- n 1) (expand-by-rules syms rules-for-fractal-plant))
     )
  )
)


(define (tree n)
  (let loop ((n n) (syms '(0)))
     (if (zero? n)
         (turtle-for-tree syms)
         (loop (- n 1) (expand-by-rules syms rules-for-simple-fractal-tree2))
     )
  )
)


(define (turtle-for-plant syms)
   (flip-vertical
      (turtle-tulkki syms
                     (empty-scene 1200 1000) ;; (square 1000 "outline" "black")
                     (make-paikka 50 10)
                     (asteet-radiaaneiksi 60)
                     (asteet-radiaaneiksi 25)
                     "forest green"
                     (lambda (depth) 8)

      )
   )
)




;; 
(define (turtle-for-tree syms)
   (flip-vertical
      (turtle-tulkki syms
                     (empty-scene 1000 1000) ;; (square 1000 "outline" "black")
                     (make-paikka 500 10)
                     (asteet-radiaaneiksi 90)
                     (asteet-radiaaneiksi 45)
                     "red"
;;                   (lambda (depth) 30)
                     (lambda (depth) (* 300 (/ 1 (+ 1 depth))))
      )
   )
)

;; Tässä versiossa PUSH ja ↰ (vasemmalle kääntyminen) hajoitettu eri komennoiksi,
;; samoin POP ja ↱ (oikealle kääntyminen).

(define (turtle-tulkki komennot aloituskanvaasi aloituspaikka aloitussuunta kulmanmuutos väri dep2len-fun)
   (let loop ([komennot komennot]
              [kanvaasi aloituskanvaasi]
              [paikka aloituspaikka]
              [suunta aloitussuunta]
              [pino (list)]
             )
    (cond [(null? komennot) kanvaasi]
          [else
             (case (first komennot)
                [(↥ 0 1 ⇡) 
                   (let* ([syvyys (length pino)]
                          [jananpituus (dep2len-fun syvyys)]
                          [new-x (+ (paikka-x paikka) (floor (inexact->exact (* (cos suunta) jananpituus))))]
                          [new-y (+ (paikka-y paikka) (floor (inexact->exact (* (sin suunta) jananpituus))))]
                         )
                       (loop (rest komennot)
                             (scene+line kanvaasi (paikka-x paikka) (paikka-y paikka) new-x new-y väri)
                             (make-paikka new-x new-y)
                             suunta
                             pino
                       )
                   )
                ]
                [(PUSH)  (loop (rest komennot) kanvaasi paikka suunta (cons (cons paikka suunta) pino))]
                [(POP)  (loop (rest komennot) kanvaasi (car (first pino)) (cdr (first pino)) (rest pino))]
                [(↰)  (loop (rest komennot) kanvaasi paikka (+ suunta kulmanmuutos) pino)]
                [(↱)  (loop (rest komennot) kanvaasi paikka (- suunta kulmanmuutos) pino)]
                [else (loop (rest komennot) kanvaasi paikka suunta pino)] ;; Tuntematon komento? Ignoroidaan (NOP).
             )
          ]
    )
   )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vertailun vuoksi:
;; Ihan erilailla tehty puunpiirto, "hard-coded":

(define (piirräpuu-apu kuva x1 y1 nyk-kulma syvyys kulman-muutos-vasemmalle kulman-muutos-oikealle)
    (cond ((zero? syvyys) kuva)
          (else
             (define x2 (+ x1 (inexact->exact (floor (* syvyys 10 (cos (asteet-radiaaneiksi nyk-kulma)))))))
             (define y2 (+ y1 (inexact->exact (floor (* syvyys 10 (sin (asteet-radiaaneiksi nyk-kulma)))))))
             (define vasen-haara (piirräpuu-apu (add-line kuva x1 y1 x2 y2 "black") x2 y2 
                                                (+ nyk-kulma kulman-muutos-vasemmalle) (- syvyys 1)
;;                                              (modulo (+ kulman-muutos-vasemmalle kulman-muutos-vasemmalle) 60)
                                                kulman-muutos-vasemmalle
                                                kulman-muutos-oikealle
                                 )
             )
             (piirräpuu-apu vasen-haara x2 y2
                            (- nyk-kulma kulman-muutos-oikealle) (- syvyys 1)
                            kulman-muutos-vasemmalle kulman-muutos-oikealle
             )
          )
    )
)

;; (piirräpuu 500 500 90 9)

;; (piirräpuu 500 500 45 15 7)

(define (piirräpuu x1 y1 kulman-muutos-vasemmalle kulman-muutos-oikealle syvyys)
   (flip-vertical (piirräpuu-apu (square 1000 "outline" "white") x1 y1 90 syvyys kulman-muutos-vasemmalle kulman-muutos-oikealle))
)