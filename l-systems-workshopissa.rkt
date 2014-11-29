#lang racket
(require 2htdp/image)

;;
;; URL: https://github.com/karttu/RacketWorkshop/blob/master/l-systems-workshopissa.rkt
;;
;; Taso: Jo pidemmälle edistyneille, listafunktiot osattava.
;;
;; Aihe: L-systems. Katso esimerkiksi: http://fi.wikipedia.org/wiki/L-systeemi (hyvin abstrakti selitys)
;;  tai englanniksi: http://en.wikipedia.org/wiki/L-system (hiukan maanläheisempi).
;;
;; Yleisempi teema: "Domain Specific Languages" (DSL), uutta näkökulmaa ohjelmien suunnitteluun, kokeneillekin ohjelmoijille.
;;
;; Tämä versio: toteuttaa http://en.wikipedia.org/wiki/L-system#Example_2 :ssa annetun yksinkertaisen säännöstön.
;;
;; Copyright (C) 2014, Antti Karttunen
;;
  
(define rules-for-simple-fractal-tree
  '(
    (0   1 L 0 R 0) ;; 0 muuttuu kaikkialla sekvenssiksi 1 L 0 R 0
    (1   1 1)       ;; 1 muuttuu kaikkialla sekvenssiksi 1 1
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

;; Nämä testauksen vuoksi:
(define puu1 (expand-by-rules '(0) rules-for-simple-fractal-tree))
(define puu2 (expand-by-rules puu1 rules-for-simple-fractal-tree))
(define puu3 (expand-by-rules puu2 rules-for-simple-fractal-tree))
(define puu4 (expand-by-rules puu3 rules-for-simple-fractal-tree))
(define puu5 (expand-by-rules puu4 rules-for-simple-fractal-tree))
;; Esimerkiksi: 
;; puu3 pitäisi olla lista: '(1 1 1 1 L 1 1 L 1 L 0 R 0 R 1 L 0 R 0 R 1 1 L 1 L 0 R 0 R 1 L 0 R 0)

;; Kokeile myös: (turtle-tulkki puu4)

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
                ((L) (loop kanvaasi
                           (rest komennot)
                           paikka
                           (+ suunta (asteet-radiaaneiksi 45))
                           (cons (cons paikka suunta) pino) ;; push paikka&pino
                     )
                )
                ((R) (loop kanvaasi
                           (rest komennot)
                           (car (first pino)) ;; Pinon päällimmäisestä paikka
                           (- (cdr (first pino)) ;; Pinon päällimmäisestä suunta
                              (asteet-radiaaneiksi 45) ;; jota 45 oikealle.
                           )
                           (rest pino) ;; dropataan pinon päällimmäinen.
                     )
                )
                (else ;; Tuntematon komento
                  (loop kanvaasi (rest komennot) paikka suunta pino)
                )
              )
           ]
     )
  )
)

