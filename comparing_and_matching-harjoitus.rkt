#lang racket



(define read-line read)

;; Kirjoitetaan funktio (listat-samanlaiset? lista1 lista)
;; jonka pitäisi toimia suunnilleen samoin kuin equal? toimii
;; kahden listan kanssa:

(define (listat-samanlaiset? lista1 lista2)
   (cond ;; Tähän jotain ehtoja...
         [else #f]
   )
)

(define (merkkijonot-samanlaiset? a b) (listat-samanlaiset? (string->list a) (string->list b)))


(define (simple-match-lists? pattern lista2)
   (cond ((and (null? pattern) (null? lista2)) #t)
         ((and (null? lista2) (eq? (car pattern) #\*))
            (simple-match-lists? (cdr pattern) lista2)
         )
         ((or (null? pattern) (null? lista2)) #f)
         ((eq? (car pattern) #\*)
             (or (simple-match-lists? (cdr pattern) lista2)
                 (simple-match-lists? pattern (cdr lista2))
             )
         )
         ((eq? (car pattern) (car lista2))
             (simple-match-lists? (cdr pattern) (cdr lista2))
         )
         (else #f)
   )
)




(define (simple-match? p s) (simple-match-lists? (string->list p) (string->list s)))


(define (lue-sanoja)
  (let loop ((sanat '())) ;; sanat on ensin tyhjä lista.
     (display "Kirjoita jotain ja paina enteriä. Kun haluat lopettaa, klikkaa syöttörivin oikeassa reunassa näkyvää eof-nappia.")
     (newline)
     (let ((sana (read-line))) ;; muuttuja sana = tiedostosta luettu rivi
        (if (eof-object? sana) ;; tuliko "End Of File", eli tiedoston loppu jo vastaan?
            sanat ;; jos tuli, niin palauta kaikki listaan kerätyt sanat
            (loop (cons sana sanat)) ;; Jos ei ole vielä end-of-file vaan oikea sana, niin lisää se listan _alkuun_
        )
     )
  )
)



(define (lue-sanat-listaksi tiedostonimi)
   (call-with-input-file tiedostonimi
          (lambda (inport)
              (let loop ((sanat (list))) ;; sanat on ensin tyhjä lista.
                 (let ((sana (read-line inport))) ;; muuttuja sana = tiedostosta luettu rivi
                    (if (eof-object? sana) ;; tuliko "End Of File", eli tiedoston loppu jo vastaan?
                        (reverse sanat)  ;; jos tuli, niin palauta kaikki listaan kerätyt sanat (alkuperäisessä järjestyksessä)
                        (loop (cons sana sanat)) ;; Jos ei ole vielä end-of-file vaan oikea sana, niin lisää se listan _alkuun_
                    )
                 )
              )
          )
   )
)

;; (define sanat (lue-sanat-listaksi "C:/Users/karttu/A/matikka/Schemuli/Racket_examples/Syksy2014/words"))

;; Miten EI pidä tehdä tätä hommaa:

(define (lue-sanat-hitaasti tiedostonimi)
   (call-with-input-file tiedostonimi
          (lambda (inport)
              (let loop ((sanat (list))) ;; sanat on ensin tyhjä lista.
                 (let ((sana (read-line inport))) ;; muuttuja sana = tiedostosta luettu rivi
                    (if (eof-object? sana) ;; tuliko "End Of File", eli tiedoston loppu jo vastaan?
                        sanat  ;; jos tuli, niin palauta lista sanat joka kerättiin hitaasti ja vaivalla
                        (loop (append sanat (list sana))) ;; Jos ei ole vielä end-of-file vaan oikea sana, niin lisää se listan _loppuun_
                    )
                 )
              )
          )
   )
)

;; Vertaa:
;;
;; > (time (begin (lue-sanat-listaksi "C:/Users/karttu/A/matikka/Schemuli/Racket_examples/Syksy2014/words") #t))
;; cpu time: 31 real time: 27 gc time: 0

;; > (time (begin (lue-sanat-hitaasti "C:/Users/karttu/A/matikka/Schemuli/Racket_examples/Syksy2014/words") #t))
;; cpu time: 277011 real time: 277659 gc time: 213189
;;


;; (filter (lambda (s) (simple-match? "*wkw*" s)) sanat)

;; (filter (lambda (s) (simple-match? "*a*a*a*a*a*" s)) sanat)

;; (filter (lambda (s) (simple-match? "k*kk*k" s)) sanat)



