#lang racket
(require 2htdp/image)


(define (sierpinski n)
   (cond
     ((zero? n) (triangle 2 "solid" "red"))
     (else
        (define pienempi (sierpinski (- n 1)))
        (above pienempi (beside pienempi pienempi))
     )
   )
)

;; Kopioitu täältä:
;; http://docs.racket-lang.org/teachpack/2htdpimage-guide.html#%28part._.Recursive_.Image_.Functions%29

(define (sierpinski-carpet n)
    (cond
      ((zero? n) (square 1 "solid" "black"))
      (else
       (local ((define c (sierpinski-carpet (- n 1)))
               (define i (square (image-width c) "solid" "white"))
              )
         (above (beside c c c)
                (beside c i c)
                (beside c c c))
       )
      )
    )
)

;; Tästä lähdettiin liikkeelle:
(define (joku-fraktaali0 n kulma)
   (cond
     ((zero? n) (circle 2 "outline" "forestgreen"))
     (else
        (define pienempi (rotate kulma (joku-fraktaali0 (- n 1) kulma)))
        (above pienempi (beside pienempi pienempi))
     )
   )
)


;; Uudempi versio:

;; Nimetyt värit löytyvät täältä:
;; http://docs.racket-lang.org/draw/color-database___.html
;; Muuten niitä voi luoda kutsulla (make-color red green blue)
;; jossa red, green ja blue ovat kokonaislukuja välillä 0 - 255.
;; Esimerkiksi (make-color 255 0 0) = "red"
;; (make-color 255 255 255) = valkoinen,
;; (make-color 0 0 0) = musta,
;; (make-color 128 128 128) = harmaa.

(define (plus-tai-miinus n) (if (odd? n) -1 1))

;; Go wild!, vaihtele kulmaa jollakin tavalla:
(define (joku-fraktaali1 n kulma väri)
   (cond
     ((zero? n) (circle 2 "outline" väri))
     (else
        (define pienempi (joku-fraktaali1 (- n 1) kulma väri))
        (above (rotate kulma pienempi)
               (beside (rotate (- (* (plus-tai-miinus n) kulma)) pienempi)
                       (rotate (* (plus-tai-miinus n) kulma) pienempi)
               )
        )
     )
   )
)

;;;;;;;;;;;;;;;;;;;;;;

(define (spin-alot kuva kulman-lisäys)
   (spin-more kuva kuva 0 kulman-lisäys)
)

(define (spin-more alkukuva kuva kulma kulman-lisäys)
     (cond
         [(>= kulma 360) kuva]
         [else
              (spin-more alkukuva
                         (overlay kuva (rotate kulma alkukuva))
                         (+ kulma kulman-lisäys)
                         kulman-lisäys
              )
         ]
     )
)

(define palkki1 (rectangle 12 120 "solid" (color 255 0 0 1)))

(define ympyra (circle 60 "outline" (make-color 255 0 0 10)))
(define kolmio (rotate 60 (above ympyra (beside ympyra ympyra))))

(define ympyra2 (circle 60 "outline" (make-color 255 0 0 60)))
(define kolmio2 (rotate 60 (above ympyra2 (beside ympyra2 ympyra2))))

;; Kokeile:
;; (spin-alot (beside (rectangle 250 2 "outline" (make-color 255 255 255 0)) kolmio2) 1)


;; (spin-alot (beside (rectangle 50 2 "outline" (make-color 255 255 255 0)) kolmio) 1)

;; (spin-alot (beside (rectangle 250 2 "outline" (make-color 255 255 255 0)) kolmio2) 1)

