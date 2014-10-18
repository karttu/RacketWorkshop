#lang racket
(require 2htdp/image)


  
(define rules-for-simple-fractal-tree
  '(
    (0   1 L 0 R 0) ;; 0 muuttuu kaikkialla sekvenssiksi 1 L 0 R 0
    (1   1 1)      ;; 1 muuttuu kaikkialla sekvenssiksi 1 1
   )
)



  
(define (expand-by-rules symbols rules)
   (let loop ([symbols (reverse symbols)] ;; käydään vanhaa tilannetta lopusta alkuun
              [new-symbols (list)] ;; ja rakennetaan uutta edestäpäin.
             )
      (cond [(null? symbols) new-symbols] ;; Tultiinko loppuun?
            [else
              (let ([rule-found (assoc (first symbols) rules)])
                 (if rule-found ;; Löytyi korvaussääntö.
                     (loop (rest symbols) ;; Skipataan vanhaa tilannet yhdellä
                           (append (rest rule-found) new-symbols)
                     )
;; Muuten, jos symbolia ei löytynyt säännöistä,
;; silloin kopioidaan ko. symboli sellaisenaan uuteen listaan:
                     (loop (rest symbols)
                           (cons (first symbols) new-symbols)
                     )
                 )
              )
            ]
      )
   )
)

(define (asteet-radiaaneiksi kulma) (/ (* pi kulma) 180))

(define-struct paikka (x y))

(define (turtle-tulkki komennot)
  (let loop ([kanvaasi (empty-scene 800 400)]
             [komennot komennot]
             [paikka (make-paikka 400 390)]
             [suunta (asteet-radiaaneiksi 90)]
             [pino (list)]
            )
     (cond [(null? komennot) kanvaasi]
           [else
              (case (first komennot)
                ((0 1) ;; Piirrä jana
                  (let ([new-x (+ (paikka-x paikka) (* (cos suunta) 20))]
                        [new-y (+ (paikka-y paikka) (* (sin suunta) -20))]
                       )
;;                     (display (format "x=~s y=~s new-x=~s new-y=~s\n"
;;                                      (paikka-x paikka) (paikka-y paikka)
;;                                      new-x new-y))
                     (loop (scene+line kanvaasi
                                       (paikka-x paikka) (paikka-y paikka)
                                       new-x new-y
                                       "red"
                           )
                           (rest komennot)
                           (make-paikka new-x new-y)
                           suunta
                           pino
                     )
                  )
                        
                )
;;                ((L) ...)
;;                ((R) ...)
                (else ;; Tuntematon komento
                  (loop kanvaasi (rest komennot) paikka suunta pino)
                )
              )
           ]
     )
  )
)

