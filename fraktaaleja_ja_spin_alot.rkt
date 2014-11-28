#lang racket
(require 2htdp/image)

;; Löytyy osoitteella: https://github.com/karttu/RacketWorkshop/blob/master/fraktaaleja_ja_spin_alot.rkt
;; Aihe: fraktaalien ja muiden mielenkiintoisten kuvioiden piirtäminen, yksinkertaisilla rekursiivisilla ohjelmilla.
;; Taso: alkeet.

;; Esimerkit on lainattu dokumentista:
;; http://docs.racket-lang.org/teachpack/2htdpimage-guide.html
;; jossa myös selitetään funktiot above ja beside

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sierpinskin kolmion piirto. Kokeile esimerkiksi: (sierpinski 7)


(define (sierpinski n)
   (cond
     [(zero? n) (triangle 2 "solid" "red")] ;; Jos ollaan saavuttu "nollatasolle" niin palautetaan kahden pikselin kokoinen kolmio
     [else
        (define pienempi (sierpinski (- n 1)))
        (above pienempi (beside pienempi pienempi))
     ]
   )
)

;; Kopioitu täältä:
;; http://docs.racket-lang.org/teachpack/2htdpimage-guide.html#%28part._.Recursive_.Image_.Functions%29
;; (ja modifioitu hiukan).

;; Kokeile esimerkiksi: (sierpinski-carpet 5) tai (sierpinski-carpet 6)
;; Kestää hetken.

(define (sierpinski-carpet n)
    (cond
      [(zero? n) (square 1 "solid" "black")] ;; Jos ollaan tultu "pohjalle", palautetaan vain musta piste.
      [else ;; Muuten luodaan matto kahdeksaan kertaan kopioidusta pienemmästä vastaavasta:
       (let* ([c (sierpinski-carpet (- n 1))] ;; Luodaan yhtä alemman/syvemmän tason (pienempi) fraktaalimatto.
              [o (square (image-width c) "solid" "white")] ;; Tämä on keskelle tuleva samankokoinen tyhjä valkoinen neliö.
             )
          (above (beside c c c)
                 (beside c o c) ;; Miten kuvio muuttuu, jos siirrät o:n keskeltä vaikkapa kulmaan?
                 (beside c c c)
          )
       )
      ]
    )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Muita esimerkkejä, kuinka pienimuotoisella tuunaamiselle saa helposti
;; mielenkiintoisia muotoja aikaiseksi.


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

;; Kokeile esimerkiksi: (joku-fraktaali1 6 17 "forest green")
;; Tai: (joku-fraktaali1 7 3 "red") (Saat "virkatun ja virttyneen version" Sierpinskin kolmiosta).

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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

;; (spin-alot (beside (rectangle 50 2 "outline" (make-color 255 255 255 0)) kolmio) 1)

;; (spin-alot (beside (rectangle 250 2 "outline" (make-color 255 255 255 0)) kolmio2) 1)

(spin-alot (beside (rectangle 150 2 "outline" (make-color 255 255 255 0)) kolmio2) 1)

