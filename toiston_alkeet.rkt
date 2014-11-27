#lang racket

;;
;; Toisto (luupit eli ohjelmasilmukat) ja miten se luontevimmin tehdään Racket-kielellä.
;;
;; Kirjoittanut 27. marraskuuta 2014, Antti Karttunen / Innokas-verkosto,
;; pohjautuen aikaisempaan, Helsingin Työväenopistolla pitämääni Racket-kurssiin.
;;
;; "Työväenopiston kalvot" joihin useasti viitataan, löytyvät osoitteella:
;; https://github.com/karttu/RacketWorkshop/blob/master/Racket_johdanto_pe279ilta1738.pdf
;;

;;
;; Kaikki tässä moduulissa annettu esimerkkiohjelmakoodi on julkisesti jaossa,
;; ja sitä saa kukin vapaasti lähteä muuttelemaan haluamaansa suuntaan.
;;

;; Kysymyksiä? Minuun saa yhteyden osoitteella Etunimeni.Sukunimeni@gmail.com
;; (mutta ei tietenkään ihan noin kirjaimellisesti). Kysykää, jos jokin kohta
;; jää epäselväksi tai vaivaamaan. Tämä materiaali myös kehittyy pikkuhiljaa.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Tapaus 0, ei vielä toistoa:

;; Aloitimme tunnilla toiston miettimisen yksinkertaisesta funktiosta, nimeltä leiki-kissaa,
;; jossa ensimmäisessä versiossa ei vielä ollut toistoa ollenkaan:

(define (leiki-kissaa-kerran) (display "Miau!"))

;; Kutsumalla funktiota: (leiki-kissaa-kerran), se kutsuu edelleen display-funktiota,
;; joka tulostaa merkkijonon "Miau!" (ilman lainausmerkkejä), ja lopettaa heti, palauttamatta
;; mitään varsinaista arvoa.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Tapaus 1, hallitsematon toisto:

;; Seuraavaksi mietimme, millaisella käskyllä opettajan voisi hypnotisoida kissaksi siten, että
;; hän jää mouruamaan loputtomiin. Mutta itse hypnotisointikäskyn pitää olla rajallisen pituinen!
;; Siis käsky tyyliin "Sano 'Miau! Miau! Miau! Miau! Miau! Miau! ...'" ei kelpaa.

;; Se saatiin aikaiseksi tällaisella käskyllä:
;; "Kun kuulet sanat 'leiki-kissaa', sano kovaan ääneen 'Miau!', jonka jälkeen käske hiljaa itseäsi: 'leiki-kissaa'".

;; Tätä lähinnä vastaava Racket-ohjelma on seuraavanlainen:

(define (leiki-kissaa)
  (display "Miau! ")
  (leiki-kissaa)
)

;; Eli ohjelma/funktio nimeltä leiki-kissaa sanoo "Miau!", jonka jälkeen se kutsuu itse itseään,
;; aina uudestaan. Koska ohjelmassa ei ole mitään lopetusehtoa, ruudun alapuoli täyttyy "Miau! Miau! Miau! ..."
;; tekstista, kunnes käyttäjä klikkaa yläkulmassa olevaa punaista stop-nappia.

;; Kyseistä tapaa tehdä ohjelmasilmukka (siis suorittaa koodia toistuvasti),
;; kutsutaan teknisessä kielessä "häntärekursioksi". Varsinaisesti se ei kuitenkaan
;; liity kissoihin, eikä sinun tarvitse toistaiseksi tietää asiasta enempää.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Tapaus 2, hallittu toisto, tulosta-luvut-ännästä-yhteen.


;; Edellisestä tehtävästä näimme, että toiston aikaansaaminen on hyvinkin helppoa.
;; Pahana puutteena maukuvassa kissassa oli kuitenkin se, ettei se lopeta koskaan,
;; muuta kuin punaista stop-nappia painamalla. Jotta saisimme aikaan hallittua toistoa,
;; tarvitsemme ehdollisesti suoritettavia käskyjä.
;;
;; Yksinkertaisin niistä on "if", joka haluaa yhden ehdon ja sen jälkeen kaksi vaihtoehtoa,
;; joita kutsutaan "then-" ja "else-haaroiksi" (suomeksi "sitten" ja "muuten-haarat"):
;; Mikäli ehto-lauseke on tosi (#t eli "true"), tehdään tai lasketaan toisena annetun then-haaran asiat,
;; muussa tapauksessa (mikäli ehto-lauseke on epätosi, #f eli "false" eli valetta) tehdään tai lasketaan
;; kolmannessa, eli "else-haarassa" annetut asiat.

;; if-lause on selitetty myös Työväenopiston kurssin kalvoissa sivulla 26.

;; Yleensä if-lauseen ensimmäisenä jäsenenä (ehtolauseena) on jokin ns. "predikaattifunktio", jonka
;; nimi päättyy usein kysymysmerkkiin, muistutuksena siitä, että ne palauttavat toden tai epätoden,
;; ja että niiden pääkäyttötarkoitus on juurikin testata jotain asiaa.

;; zero? ja eräitä muita predikaattifunktioita on esitelty kyseisissä kalvoissa sivuilla 20-22.

;; Lisäksi tarvitsemme erikoismuotoa begin, jolla useampia funktiokutsuja voi ryhmitellä yhdeksi lausekkeeksi,
;; sillä if-lauseen else-haaran pitää olla yksi lauseke.
;; (begin-lause on selitetty kalvojen sivulla 25).


(define (tulosta-luvut-ännästä-yhteen n)
  (if (zero? n)
      "valmis"       ;; Then-haara, arvo joka palautetaan jos n on nolla, tässä tapauksessa merkkijono "valmis"
      (begin         ;; Else-haara alkaa tästä
        (display n)    ;; Jossa ensin tulostetaan luvun n arvo.
        (newline)      ;; ja rivinvaihto sen perään.
        (tulosta-luvut-ännästä-yhteen (- n 1)) ;; Ja luupataan takaisin "alkuun", yhtä pienemmällä n:n arvolla.
      )
  )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Tapaus 3, toiston soveltaminen opetuksessa, yhteenlasku-visa.


;; Seuraava funktio on pieni interaktiivinen ohjelma, joka kyselee
;; käyttäjältä mitä kahden random-funktiolla arvotun luvun summa on.
;; Jos käyttäjä vastaa oikein, onnittelee ja lopettaa,
;; muuten luuppaa alkuun ja kysyy uudestaan, eri luvuilla.

(define (yhteenlasku-opetus vaikeustaso)
   (define luku1 (random vaikeustaso)) ;; Arvo luku1
   (define luku2 (random vaikeustaso)) ;; ja luku2
;; Kysytään käyttäjältä kysymys, käytetään format-funktiota muotoiluun:
   (display (format "Paljonko on ~a plus ~a ?" luku1 luku2))
   (newline)
;; Lukee käyttäjän vastauksen muuttujaan nimeltä vastaus:
   (define vastaus (read))
;; Ja seuraavaksi tarkistaa onko se yhtäsuuri kun luku1+luku2 ?
   (if (equal? vastaus (+ luku1 luku2))
;; Jos oli, niin onnittele ja lopeta:
       (display "Oikein meni, kiitos ja näkemiin!")
;; Muuten kerro ettei ollut:
       (begin (display "Trööt, eipä ollut! Saat uuden yrityksen.")
              (newline)
;; ja luuppaa takaisin alkuun kutsumalla itseään, vaikeustaso pidetään
;; samana.
              (yhteenlasku-opetus vaikeustaso)
       )
   )
)


;; Ohjelma käynnistyy kutsumalla sitä esimerkiksi tulkki-ikkunasta (yhteenlasku-opetus 25)

;; UUSIA FUNKTIOITA:
;;
;;   (read)  - lukee käyttäjältä jonkin asian (esimerkiksi kokonaisluvun) ja palauttaa sen tuloksenaan.
;;             Selitetty kalvojen sivulla 19.
;; 
;;   (format "..." luku1 luku2)
;;           - on lähinnä display-funktion apufunktio, jolla saadaan muuttujien luku1 ja luku2 arvot
;;             ujutettua sopiviin kohtiin käyttäjälle tulostettavaa viestiä, jonka sisälle ne on merkattu ~a :illa.
;;  
;;   (equal? a b) - testaa ovatko asiat a ja b samanlaiset. Jos ollaan varmoja, että a ja b ovat aina
;;                  lukuja, voitaisiin käyttää myös funktiota (= a b). Selitetty kalvojen sivulla 20.
;; 
;;   (random n)   - missä n on jokin nollaa suurempi kokonaisluku, palauttaa jonkin satunnaisesti valitun
;;              luvun väliltä 0 - (n-1). Esimerkiksi (random 6) voi palauttaa jonkin luvuista 0, 1, 2, 3, 4 tai 5,
;;              joka tuotetaan niin kutsutulla "valesatunnaislukugeneraattorilla".
;; 
;; 
;; LISÄHARJOITUS:
;;
;;   Mitä kaikkia kohtia ylläolevassa funktiossa/ohjelmassa pitää muuttaa, että se
;;   kysyykin kahden luvun yhteenlaskun sijasta kertolaskun tulosta?
;;   (Ainakin funktion nimi: kertolasku-visa, ja sen lisäksi (ainakin) kolmea muuta kohtaa.
;;    Funktiolla * saa kerrottua lukuja, esimerkiksi (* 1 2 3 4) antaa tulokseksi 24).
;; 
;; 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Tapaus 4, toiston soveltaminen opetuksessa, arvuuta käyttäjää arvaamaan oikea luku.

;; Seuraava funktiota on tarkoitus kutsua tulkki-ikkunasta esimerkiksi näin:
;; (arvaa-luku (random 100))
;; Huomaa kuinka (random 100) arpoo satunnaisen kokonaisluvun väliltä 0 - 99,
;; mutta ei näytä sitä käyttäjälle, vaan antaa sen suoraan arvuuttavalle arvaa-luku
;; funktiolle parametrina (siis kyseisen funktion argumenttina).
;;
;; Tämän jälkeen ohjelma arvuuttelee käyttäjältä aina uudestaan ja uudestaan kyseistä
;; lukua ja lopettaa vasta sitten kun käyttäjä on arvannut sen oikein.
;; Mikäli käyttäjä (tai "pelaaja") ei arvannut lukua oikein, niin ohjelma kertoo vain
;; sen onko oikea luku pienempi vai suurempi kuin käyttäjän antama, ja antaa hänen arvata
;; uudestaan.


;; Varsinaista ohjelmakoodia lyhentääksemme määrittelemme pienen
;; apufunktion nimeltä display-nl joka tulostaa aina yhden
;; rivinvaihdon (engl. "newline", uusi rivi) tulostamansa asian perään:

(define (display-nl jutska) (display jutska) (newline))


(define (arvaa-luku arvattava-luku)
    (display "Arvaa salainen lukuni!")
    (define arvaus (read)) ;; Luetaan käyttäjältä luettu luku "funktionsisäiseen apumuuttujaan" nimeltä arvaus
    (if (equal? arvaus arvattava-luku) ;; Jos arvaus oli oikein ...
        (display-nl "Onneksi olkoon, tuo se oli!") ;; niin tulostetaan "Onneksi olkoon" viesti ja lopetetaan.
        (begin ;; Muussa tapauksessa if-lauseen else-haarassa tarkistetaan ensin cond-lauseella vielä lisää:
;;                oliko arvaus pienempi, suurempi kuin arvattava-luku, tai yleensä kokonaisluku ollenkaan:
          (cond
             [(not (integer? arvaus))
                 (display-nl "Hyvää päivää kirvesvartta?! Sinun piti antaa jokin kokonaisluku!")
             ]
             [(< arvaus arvattava-luku) (display-nl "Suurempi kuin tuo.")]
             [else (display-nl "Pienempi kuin tuo.")]
          )
          (arvaa-luku arvattava-luku) ;; Jonka jälkeen palataan alkuun samalla luvulla.
        )
    )      
)


;; Ohjelman voi käynnistää myös tällä (arvaa-salainen-luku) funktiolla, jolloin arvuutetaan
;; lukua välillä 1 - 100:
;; Huomaa kuinka (+ 1 ...) tarvitaan lisäämään random-funktioon palauttamaan 0-99 välillä olevaan lukuun yksi:

(define (arvaa-salainen-luku) (arvaa-luku (+ 1 (random 100))))



;; UUSIA FUNKTIOITA:
;;
;;   cond on hivenen mutkikkaampi muoto if-lauseesta. Lyhykäisesti tämä on selitetty esimerkiksi
;;   Työväenopiston kalvojen sivulla 27. Lisäksi ylläolevassa koodissa käytetään cond-lauseen
;;   ehtolausekkeiden ulompina sulkuina hakasulkuja [] tavallisten sulkujen () asemasta. Tämä on vain
;;   ihmissilmälle tarkoitettua "syntaktista sokerointia", jolloin ohjelmoijan on helpompi erottaa
;;   erilaisen tehtävän omaavat sulut toisistaan.
;;   Ylläolevan voisi kirjoittaa tavallisillakin suluilla, näin:
;;
;;      (cond
;;            ((not (integer? arvaus))
;;               (display-nl "Hyvää päivää kirvesvartta?! Sinun piti antaa jokin kokonaisluku!")
;;            )
;;            ((< arvaus arvattava-luku) (display-nl "Suurempi kuin tuo"))
;;            (else (display-nl "Pienempi kuin tuo"))
;;      )
;; 


;;
;; MIETITTÄVÄÄ:
;;
;;  (1)  Uskoisin että moni hoksaa varsin nopeasti, mikä on hyvä "haarukointistrategia" kyseistä
;;       peliä "pelattaessa". Harjoituksen ironia onkin siinä, että kyseisessä pelissä käyttäjä pärjää parhaiten
;;       "matkimalla itse tietokonetta", eli soveltamalla puolitushakua (englanniksi "binary search algorithm").
;;       Ei välttämättä noin matemaattisen tarkkaan kuin sivulla http://fi.wikipedia.org/wiki/Puolitushaku
;;       vaan nopeaan ratkaisuun riittää että pelaaja osaa puolittaa hakualueen päässään edes suunnilleen.

;; 
;;  (2)  Kahdessa viimeisimmässä esimerkissä ei voi välttyä huomaamasta, että tällaisille interaktiivisille
;;       ohjelmille tulee pakostakin luoneeksi jonkinlaisen "persoonallisuuden", ihan pelkästään sillä,
;;       miten valitsee ne sanamuodot, joilla ohjelmoija kommunikoi käyttäjän kanssa (virallinen? ystävällinen?
;;       töykeä? ilkikurinen?)
;;
;;       Ohjelman muutkin piirteet voivat vaikuttaa huomattavasti sen "luonteeseen".
;;       Mitä esimerkiksi olisi ajateltava ylläesitetyn ohjelman reiluudesta, mikäli
;;       se kutsuisikin itseään uudestaan (cond-lauseen jälkeen) vaikkapa näin:
;;         (arvaa-luku (random 1000))
;;       sen sijaan, että pitäisi luvun arvattava-luku samana?
;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Tapaus 5, toiston soveltaminen, kielen opetus


;; Seuraava "englanninkielen opetus-ohjelma" kysyy käyttäjältä
;; mitä tietty englannin sana on suomeksi, ja lopettaa
;; jos käyttäjä vastaa oikein. Muuten kysyy seuraavaa paria
;; (esim. octopus - mustekala parin jälkeen paria
;;  turmeric - kurkuma, jne.
;;  viimeisen parin (crayfish - jokirapu) jälkeen 
;; alkaa uudestaan alusta):

;; Käynnistetään kutsumalla tulkki-ikkunasta (kielen-opetus-eka-harjoitus)

(define (kielen-opetus-eka-harjoitus)
  (kielen-opetus-seitsemälle-sanaparille
      "octopus"   "mustekala"
      "turmeric"  "kurkuma" 
      "cat"       "kissa"
      "fish"      "kala"
      "catfish"   "kissakala"
      "rayfish"   "rausku"
      "crayfish"  "jokirapu"
  )
)


(define (kielen-opetus-seitsemälle-sanaparille eng1 suomi1 eng2 suomi2 eng3 suomi3 eng4 suomi4 eng5 suomi5 eng6 suomi6 eng7 suomi7)
    (display-nl (format "Mitä englannin sana ~a on suomeksi?" eng1))
    (define vastaus (read-line))
    (if (equal? vastaus suomi1)
        (display-nl "Oikein meni, kiitos ja näkemiin, olet tosi hyvä!")
        (begin 
           (display-nl "Trööt, eipä ollut! Saat uuden yrityksen.")
           (kielen-opetus-seitsemälle-sanaparille
              eng2 suomi2 eng3 suomi3 eng4 suomi4 eng5 suomi5 eng6 suomi6 eng7 suomi7 eng1 suomi1
           )
        )
    )
)

;;
;; Huomioita:
;;  Kyseisessä pikkuohjelmassa sanalistojen pyörittäminen tehdään niin, että funktiossa
;;  kielen-opetus on yhtä monta eng-n suomi-n argumenttiparia kuin kyseltäviä sanojakin on,
;;  ja kun tehdään häntärekursiivinen kutsu käyttäjän epäonnistuneen arvauksen jälkeen,
;;  niin ensimmäinen argumenttipari eng1 suomi1 vain siirretään argumenttilistan loppuun,
;;  jolloin seuraavalla kerralla ne sanat jotka olivat alunperin eng2 ja suomi2
;;  ("turmeric" ja "kurkuma"), ovatkin muuttujissa eng1 ja suomi1, ja kaikki muutkin
;;  sanaparit (eng1 ja suomi1 paria lukuunottamatta) siirtyvät automaattisesti yhden
;;  parin verran eteenpäin.
;;
;;  Tähän tyyliin koodattua ohjelmaa on kuitenkin hankala muutella, esimerkiksi jos halutaan
;;  kyseltävään settiin lisä yksi sanapari lisää, vaikkapa "parrot" ja "papukaija", niin silloin
;;  pitäisi lisätä funktion jo entuudestaan pitkään argumenttilistaan vielä kaksi argumenttia
;;  lisää: eng8 suomi8, ja muistaa lisätä ne myös "häntäkutsuun", oikeaan paikkaan, parien
;;  eng7 suomi7 ja eng1 suomi1 väliin.
;;
;;  Tällaiset ohjelmat onkin parempi kirjoittaa käyttäen hyväkseen Racketistä valmiina löytyviä
;;  listafunktioita, josta pääsemme seuraavaan harjoitukseen:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Tapaus 6, kielen opetus fiksummin, listafunktioita apuna käyttäen.

;; Epämuodollinen määritelmä lista-tietotyypille:
;;   Lista on Racket:ille ja muille Lisp-ryhmän kielille tyypillinen tietorakenne,
;;   jossa on järjestyksessä nolla tai useampia mitä tahansa juttuja. "Mitä tahansa juttuja"
;;   tosiaan tarkoittaa sitä, eli listassa voi alkioina olla vaikkapa kuvia tai toisia listoja!
;;
;; Esimerkkejä:
;;
;;    tyhjä lista:  '()    [Tämä on varmasti tärkein kaikista listoista, samalla tavalla kuin nolla on tärkein luku!]

(define tyhjä-lista '())

;;
;; Lista jossa on luvut yksi, kaksi ja kolme:  '(1 2 3):

(define lista123 '(1 2 3))
(define lista_un_dos_tres (list 1 2 3)) ;; Luo samanlainen lista, mutta käyttäen apuna funktiota list

;;
;; Lista jonka ensimmäinen alkio on symboli sammakko, toinen yllämainittu kolmen
;; luvun lista, ja viimeisenä merkkijono "hummeri huu": '(cembalo (1 2 3) "hummeri huu")

(define vielä-yksi-lista (list 'cembalo lista123 "hummeri huu"))

;; Huomaa että ylläannetuissa esimerkeissä yksinkertainen lainausmerkki ' ei
;; varsinaisesti kuulu itse listaan, vaan sitä tarvitaan vain kertomaan Racket-tulkille,
;; ettei se yrittäisi tulkita kyseistä listaa funktiokutsuna, jota se muuten
;; erehdyttävästi muistuttaisi.


;; Kielenopetusohjelmaamme tarvitsemme seuraavanlaisen, itsemääritellyn listanpyörittelyfunktion:

(define (kierrä-listaa-vasemmalle lista)
   (append (rest lista) (list (first lista)))
)

;; Ideana tässä on yhdistää listan "hännän" (eli heti ekasta alkiosta seuraavien)
;; loppuun yhden alkion lista, joka on muodostettu alkuperäisestä ensimmäisestä alkiosta
;; kutsulla (list (first lista))

;; Listankäsittelyfunktiot:
;;
;;   first (antaa tuloksenaan listan ekan alkion, eli sen "pään"),
;;
;;   rest (antaa tuloksenaan listan joka koostuu kaikista muista kuin sen ensimmäisestä alkiosta,
;;         toisinsanoen sen "hännän"),
;;
;;   reverse (antaa tuloksenaan lopusta-alkuun käännetyn listan)
;;
;;   append (yhdistää kaksi listaa peräkkäin)
;; ja 
;;   list (luo argumenttinaan annetuista alkioista uuden listan)
;;
;; löytyvät englanniksi selitettyinä esimerkiksi URL:illa:
;; http://docs.racket-lang.org/guide/Lists__Iteration__and_Recursion.html
;;

;;  Huomaa että (first tyhjä-lista) ja (rest tyhjä-lista) siis (rest '())
;;  antavat virheilmoituksen, sillä tyhjässa listassa ei ole päätä eikä häntää.

;;
;; HARJOITUS (kokeile kirjoittaa kyseisiä kutsuja tulkki-ikkunaan):
;;
;;  Mitä (kierrä-listaa-vasemmalle lista123) antaa tulokseksi?
;;  Miltä lista123 näyttää sen jälkeen?
;;
;;  (Pitäisikö? Kyllä, sillä Racketin listankäsittelyfunktiot ovat
;;   niinsanotusti "puhtaita", tai sivuvaikutuksettomia, eli ne eivät
;;   muuta niille argumenttina annettua listaa, vaan luovat aina puhtaalta
;;   pöydältä uuden listan).
;;
;;  Mitä (kierrä-listaa-vasemmalle vielä-yksi-lista) antaa tulokseksi?
;;  
;;  Mitä (kierrä-listaa-vasemmalle (kierrä-listaa-vasemmalle lista123))
;;  antaa tulokseksi?
;;

;; HARJOITUS*: Miten tekisit vastaan kierto-funktion, nimeltään vaikkapa
;;  kierrä-listaa-oikealle, joka kiertäisi listaa toiseen suuntaan
;;  (viimeinen alkio siirretään eteen, ja kaikki muut siirtyvät yhdellä
;;   askelella kohti loppua, siten että toiseksi viimeisestä tulee viimeinen,
;;   ja ensimmäisestä toinen).
;;
;; Siis, kutsumalla
;; (kierrä-listaa-oikealle '(a b c d e f))
;; tulokseksi pitäisi tulla (f a b c d e)

;; VIHJE: Voit käyttää apunasi tuota yllämääriteltyä kierrä-listaa-vasemmalle funktiota,
;; sekä Racketista valmiiksi löytyvää reverse-funktiota, joka kääntää listan
;; lopusta alkuun, esimerkiksi (reverse '(a b c)) antaa tulokseksi: '(c b a)
;; ja näitä saa kutsua niin monta kertaa kuin haluaa tai kokee tarvitsevansa.

;; Takaisin varsinaiseen tapaukseemme. Nyt huomataan että kielen-opetus ohjelmamme
;; yksinkertaistuu huomattavasti:

(define (kielen-opetus sanat-englanniksi sanat-suomeksi)
    (display-nl (format "Mitä englannin sana ~a on suomeksi?" (first sanat-englanniksi)))
    (define vastaus (read))
    (if (equal? vastaus (first sanat-suomeksi))
        (display-nl "Oikein meni, kiitos ja näkemiin, olet tosi hyvä!")
        (begin 
           (display-nl "Trööt, eipä ollut! Saat uuden yrityksen.")
           (kielen-opetus
              (kierrä-listaa-vasemmalle sanat-englanniksi)
              (kierrä-listaa-vasemmalle sanat-suomeksi)
           )              
        )
    )
)

(define (opeta-suomi-englanti)
  (kielen-opetus 
    '(octopus    turmeric cat   fish catfish   rayfish crayfish aardvark parrot)
    '(mustekala  kurkuma  kissa kala kissakala rausku  jokirapu maasika  papukaija)
  )
)

;; Huomioita:
;;  Tässä versiossa sanat listoissa ovat symboleja, eivät merkkijonoja (niitä ei ole
;;  ympäröity lainausmerkeillä), ja siitä syystä myös vastaus käyttäjältä luetaan
;;  nyt funktiolla (read) eikä (read-line), jotta käyttäjältä luettu vastaus olisi
;;  tyypiltään symboli eikä merkkijono. Toistaiseksi sinun ei tarvitse tietää näiden
;;  tietotyyppien erosta muuta kuin sen että toisen ympärillä on lainausmerkit, ja toisen ei.
;;  Symbolissa ei voi myöskään olla välilyöntejä, yksöislainausmerkkejä ja eräitä
;;  muita erikoismerkkejä. Merkkijonoissa ne on kaikki sallittuja. Esimerkiksi
;;  O'Malley ei toimi symbolina, mutta "O'Malley" toimii merkkijonona.
;;

;; Katso seuraavaksi myös toiston_alkeet_kuvilla.rkt jossa on samantapainen arvuutusohjelma,
;; mutta joka arvuuttelee eläinten kuvia. Huom! Koska kyseisessä lähdekoodissa on mukana kuvia,
;; se ei näy oikein kuin vasta sitten kun se avataan DrRacket-ympäristössä.
;;






      
