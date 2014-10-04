#lang racket
(require 2htdp/image)


;; Mitä muunmuassa käytiin lauantaina 4.10.2014 läpi:

;; (Tämä versio ilman kuvia, jotta ei tarvita Racket-ympäristöä tiedoston lukemiseen).

;; Seuraava funktiota voi kutsua esimerkiksi näin: (hei "Aapo")
;; tai (hei "")

(define (hei nimi)
   (display "Hei ")
;; (if (equal? nimi "") (display "Nimetön") (display nimi)) ;; Tämä rivi kommentoitu, koska ...
   (display (if (equal? nimi "") "Nimetön" nimi)) ;; ... vaikutus on sama kuin tällä rivillä.
   (display " Mitä kuuluu?")
   (newline)
)

;; häntärekursio: n pitää olla 1 tai suurempi kokonaisluku, kissa maukaisee niin monta kertaa:

(define (käyttäydy-kuin-kissa n)
  (cond [(= 1 n) (display "Viimeinen maukaisu!") (newline)]
        [else
          (display "Mau! ")
          (display n)
          (newline)
          (käyttäydy-kuin-kissa (- n 1)) ;; "Kissaksi hypnotisoitu mies" komentaa itse itseään... (demosin tunnilla)
        ]
  )
)


;; Lisää häntärekursiivisia funktioita
;; Kutsutaan esimerkiksi (arvaa-luku (random 100))

(define (arvaa-luku jokuluku)
   (define arvaus (read))
   (cond [(equal? arvaus jokuluku) "Oikein meni, onneksi olkoon!"]
         [(< jokuluku arvaus)
             (display "Pienempi kuin tuo. Yritä uudestaan!")
             (newline)
             (arvaa-luku jokuluku)
         ]
         [else
             (display "Suurempi kuin tuo. Yritä uudestaan!")
             (newline)
             (arvaa-luku jokuluku)
         ]
   )
)

;; Kyllästyimme kirjoittamaan aina (display jotain) (newline) peräkkäin,
;; joten määrittelimme oman funktion nimeltä display-nl joka laittaa
;; rivinvaihdon automaattisesti näytetyn asian perään:
(define (display-nl jutska) (display jutska) (newline))


;; Kutsutaan esimerkiksi (kertolasku-visa 10)

(define (kertolasku-visa vaikeustaso)
   (define luku1 (random vaikeustaso))
   (define luku2 (random vaikeustaso))
   (display "Paljonko on ")
   (display luku1)
   (display " kertaa ")
   (display luku2)
   (display-nl " ?")
   (define vastaus (read))
   (cond
;; Jos vaikeustaso vähennettiin jo nollaan ilman että käyttäjä vastasi kertaakaan oikein, joten lopetetaan.
      [(zero? vaikeustaso) (display "Ei tänään, ilmeisestikään?")]
      [(= vastaus (* luku1 luku2)) (display-nl "Oikein meni, onneksi olkoon!")]
      [else
        (display-nl "Väärin meni, yritä uudestaan!")
        (kertolasku-visa (- vaikeustaso 1)) ;; Pienennetään vaikeustasoa yhdellä.
      ]
   )
)

;; Listojen käsittelyä.
(define luvut (list 1 22 333)) ;; Luodaan kolmen kokonaisluvun lista, nimeltä luvut antamalla funktiolle list kolme argumenttia.

;; Luodaan viiden elementin lista, jossa eka elementti kokonaisluku 1, toinen on merkkijono "foo",
;; kolmas elementti on kahden kokonaisluvun muodostama lista (33 5),
;; neljäs elementti on symboli ankerias,
;; ja viides elementti on taas kokonaisluku:

(define seot '(1 "foo" (33 5) ankerias 33)) 


;; Listoista voi ottaa osia funktioilla:
;;  (first seot) ;; palauttaa listan "pään" eli ekan alkion, tässä tapauksessa 1:sen.
;;  (rest seot) ;; palauttaa listan "hännän" eli kaikki muut kuin ekan alkion,
;;                 eli tässä tapauksessa listan: ("foo" (33 5) ankerias 33)

;; Huom: tyhjää listaa merkitään '() joka saadaan myös funktiokutsulla (list) kun sitä kutsutaan ilman argumentteja).
;; Tyhjä lista vanhemmissa Lispeissä kulkee myös nimellä NIL tai nil, ja on niissä oikeastaan
;; eräänlainen listojen ja symbolien rajamailla oleva kummajainen (vrt. NULL-pointteri C:ssä).

;; Huomaa että (list? seot) palauttaa #t:n (totuusarvo true:n), samoin (list? '())
;; Sen sijaan vaikka (pair? seot) palauttaa #t:n, niin (pair? '()) palauttaa #f:n (totuusarvo false:n)

;; Eli pair? on tavallaan sellainen predikaattifunktio, joka palauttaa true:n vain
;; silloin kuin sen argumentti on EPÄTYHJÄ lista.

;; Tällä erolla on merkitystä mm. sen takia, että tyhjästä listasta ei voi ottaa päätä eikä häntää,
;; vaan sekä (first '()) että (rest '()) aiheuttaa virheen.

;; Huom: joissakin vanhemmissa Lispeissä, joissa first kulkee nimellä car ja rest nimellä cdr
;; (ja näitä synonyymejä käytetään yleisesti myös Schemessä / Racketissä) saattaa olla niin
;; että on erityisesti määritelty niin että (car '()) = () ja (cdr '()) = ().
;; Lisäksi useimmissa Lispeissä (erotuksena Schemeistä ja Racketista ja Clojuresta) kyseinen
;; NIL-symboli eli tyhjä lista toimittaa myös #f:n eli epätoden totuusarvon virkaa.


;; (cons 'uusi-pää seot) ;; luo uuden listan, jossa listan seot alkuun on lisätty uusi symboli:
;; jolloin saadaan lista: (uusi-pää 1 "foo" (33 5) ankerias 33)

(define seot2 (cons 'uusi-pää seot))
;; Huom! seot pysyy ennallaan.

;; Jos luvut = (1 22 333)
;; niin (rest luvut) = (22 333)
;; ja (rest (rest luvut)) = (333)
;; ja (rest (rest (rest luvut))) = ()
;; eli näemme, että kaikki listat lopulta päättyvät tyhjään listaan, eli nilliin.

;; (append luvut seot) ;; Yhdistää listat luvut ja seot uudeksi listaksi joka palautetaan tuloksena. Vanhat listat pysyvät ennallaan.

;; (reverse luvut) ;; Kääntää listan luvut takaperin. Alkuperäinen lista luvut säilyy oikeinpäin.

;; (length lista) palauttaa listan ("ylätason") pituuden.
;; esimerkiksi (length '()) palauttaa nollan, koska tyhjässä listassa ei ole laisinkaan alkioita.
;; Mitä (length seot) palauttaa?

;; Kirjoitetaan oma versio pituus-funktiosta. Ensin ei-häntärekursiivisesti:
;; (Tunnilla tehtiin toisin päin, siksi tässä nimessä kakkonen...)

(define (pituus2 lista)
   (if (null? lista) 0  ;; Jos argumentti on tyhjä lista (), niin sen pituus on varmasti nolla.
       (+ 1 (pituus2 (rest lista))) ;; muussa tapauksessa pituus on yksi enemmän kuin listan hännän pituus.
   )
)

;; Huomataan että tämä toteutus ei ole häntärekursiivinen (kts. seuraava), sillä
;; funktio joutuu aina palauttamaan listan seuraavasta häntäpätkästä (rest ...) lasketun
;; osapituuden edelliselle tasolle, jossa siihen (rekursiosta palattaessa) lisätään vielä yksi.
;; Esimerkiksi jos kutsumme (pituus2 seot) niin, silloin tapahtuu oikeastaan seuraavanlainen
;; laskutoimitus:
;; (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 0))))) ja tulos on yhteensä 5.


;; Sama häntärekursiivisesti, kahdella funktiolla toteutettuna:
(define (pituus lista) (pituus-apu lista 0)) ;; Varsinainen funktio kutsuu heti apufunktiota:

;; ... nimeltä pituus-apu, jolle annetaan sama lista ja n:ksi ekalla kerralla nolla:
(define (pituus-apu lista n)
   (if (null? lista) n ;; Jos ollaan tultu jo listan loppuun, palautetaan n
       (pituus-apu (rest lista) (+ n 1)) ;; Muuten siirrytään yksi askel listan loppua kohden ja kasvatetaan n:ää yhdellä.
   )
)

;; Kokeile esimerkiksi (pituus seot)
;; Huomaa että rekursiivinen kutsu pituus-apu:un on "viimeisenä" (eli "häntäpositiossa")
;; if:fin else-haarassa, eikä kyseisen kutsun "ulkopuolella" (eikä if:finkään ulkopuolella)
;; tehdä enää mitään muutoksia palautetulle n:n arvolle.
;; Toisin sanoen, tulkki/kääntäjä voi samantien optimoida kyseisen
;; rekursiivisen kutsun luupiksi, eikä pino koskaan vuoda yli, oli lista
;; sitten kuinka pitkä tahansa.
;; Huom: Scheme/Racket-kielen standardi TAKAA tämän muunnoksen tekemisen AINA,
;; toisin kuin monissa muissa kielissä, joissa vastaavanlainen kutsu
;; optimointiasetuksista riippuen ehkä muutetaan luupiksi tai sitten ei.
;; Myös jälkimmäisessä tapauksessa tulos on tietysti periaatteessa sama,
;; niin kauan kuin pino ei vuoda yli, ja vaikka tulos tulisi hivenen hitaammin.


;; Kirjoitettiin muunnelma edellisestä, funktio summaa joka laskee
;; listasta lukuja niiden summan:
;; (summaa '(7 3 6)) --> 16

;; Kokeile myös (summaa luvut) ja (summaa '())


(define (summaa lista) (summaa-apu lista 0))

;; Muuten aivan samanlainen, mutta lisätään ykkösen sijasta aina
;; sen hetkisestä kohdasta ko. listanpätkän eka alkio kokonaissummaan n:

(define (summaa-apu lista n)
   (if (null? lista) n
       (summaa-apu (rest lista) (+ n (first lista)))
   )
)

;; Kirjoitettiin oma versio member-funktiosta, joka palauttaa
;; listasta lista ensimmäisen suffiksin (*) joka alkaa alkiolla alkio.
;; (*) Tässä "suffiksi" tarkoittaa listan loppuun päättyvä osalistaa.
;; Eli listan (1 2 3) suffikseja ovat (1 2 3), (2 3), (3) ja ().

(define (ourmember alkio lista)         
   (cond 
     ;; Jos lista tyhjä (jo valmiiksi tai ollaan tultu jonkun listan loppuun),
     ;; niin silloin se ei voi sisältää mitään alkiota, palautetaan siis #f eli false:
         [(null? lista) #f]
         [(equal? (first lista) alkio) lista] ;; Löytyi eka kohta listasta, jossa "pää" on alkio, palauta se kohta.
         [else (ourmember alkio (rest lista))] ;; Muussa tapauksessa etsi samaa alkiota listan hännästä.
   )
)

;; Kokeile esimerkiksi:

;; (ourmember 1 luvut)
;; (ourmember "foo" seot)
;; (ourmember "ei tätä ainakaan" luvut)
;; (ourmember '(33 5) seot)
;; (ourmember 2 '(1 2 3 2 5 1))

;; Lisäksi setvittiin funktioiden eq? ja equal? eroa,
;; Esimerkiksi (equal? '(1 2 3) '(1 2 3)) palauttaa #t:n (true:n)
;; mutta (eq? '(1 2 3) '(1 2 3)) palauttaa #f:n (false:n), koska
;; eq? vertailee vain "pointtereita", kun taas equal? käy koko listan
;; rekursiivisesti läpi.
;; Sen sijaan (eq? luvut luvut) palauttaa #t:n (true:n), koska eq?:n
;; kumpanakin argumenttina on sama pointteri, muuttuja luvut arvona olevaan listaan.

;; Yleisesti: equal? on hitaampi, mutta varmempi tapa testata samuutta, eq? on hakkerin
;; optimoitu versio, silloin kun tiedetään mitä tehdään.

;; Lisäksi kysyttiin miten voi testata kolmen asian samuutta. Se onnistuu esimerkiksi
;; seuraavanlaisella funktiolla:

(define (kolme-samaa? x y z) (and (equal? x y) (equal? y z))) ;; (and (equal? x y) (equal? x z)) toimisi myös!

