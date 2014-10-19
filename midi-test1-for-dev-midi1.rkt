
(define out #f)

(define (open-midi1) ;; Actually, /dev/midi1
  (and (not out) (open-output-file "/dev/midi1" #:mode 'binary #:exists 'append))
)

(define (midi-send-message out list-of-bytes)
   (cond ((not out) (set! out (open-midi1))))
   (for-each (lambda (byte) (write-byte byte out)) list-of-bytes)
   (flush-output out)
)

  
(define (play-something t)
   (let loop ((note 65))
     (begin 
        (midi-send-message out (list #b10010000 (modulo note 128) 127))
        (sleep t) 
        (midi-send-message out (list #b10000000 (modulo note 128) 127))
        ;; (sleep pause) 
        (loop (+ 1 note))
     )
   )
)


(define (midi-turn-on notevalue velo) (and (not (zero? notevalue)) (midi-send-message out (list #b10010000 (modulo notevalue 128) velo))))

(define (midi-turn-off notevalue velo) (and (not (zero? notevalue)) (midi-send-message out (list #b10000000 (modulo notevalue 128) velo))))



;;;;;;;;;;;;;;;;;;;;;;;


(define (list-head lista n-first)
  (if (zero? n-first) '() (cons (first lista) (list-head (rest lista) (- n-first 1))))
)

(define (sublist lista from to+1) (list-head (list-tail lista from) (- to+1 from)))

(define (euclidean_1_0_rhythm n-beats ones)
  (let ([zeros (- n-beats ones)])
   (let loop ([lists (append (make-list (min ones zeros) '(1 0))
                             (make-list (abs (- ones zeros)) (list (if (> ones zeros) 1 0)))
                     )
              ]
              [n-longer-lists (min ones zeros)]
              [n-shorter-lists (abs (- ones zeros))]
             )
      (cond [(<= n-shorter-lists 1) (apply append lists)] ;; Ready? Concatenate sublists together
            [else
               (let* ([n-to-combine (min n-longer-lists n-shorter-lists)]
                      [n-to-leave (- (max n-longer-lists n-shorter-lists) n-to-combine)]
                     )
                  (loop
                     (append
                         (map append (list-head lists n-to-combine) (list-head (reverse lists) n-to-combine))
                         (sublist lists n-to-combine (+ n-to-combine n-to-leave))
                     )
                     n-to-combine ;; On the next iteration this is the number of longer lists
                     n-to-leave   ;; On the next iteration this is the number of shorter lists
                  )
               )
            ]
      )
   )
  )
)


(define (euclidean_rhythm n-onsets n-beats) ;; Note n-onsets <= n-beats
  (let ([n-zeros (- n-beats n-onsets)])
   (let loop ([lists (append
                        (build-list (min n-onsets n-zeros) (lambda (i) (list (add1 i) 0)))
                        (build-list
                                  (abs (- n-onsets n-zeros))
                                  (lambda (i) (list (if (> n-onsets n-zeros) (+ i 1 n-zeros) 0)))
                        )
                     )
              ]
              [n-longer-lists (min n-onsets n-zeros)]
              [n-shorter-lists (abs (- n-onsets n-zeros))]
             )
      (cond [(<= n-shorter-lists 1) (apply append lists)] ;; Ready? Concatenate sublists together
            [else
               (let* ([n-to-combine (min n-longer-lists n-shorter-lists)]
                      [n-to-leave (- (max n-longer-lists n-shorter-lists) n-to-combine)]
                     )
                  (loop
                     (append
                         (map append (list-head lists n-to-combine) (list-head (reverse lists) n-to-combine))
                         (sublist lists n-to-combine (+ n-to-combine n-to-leave))
                     )
                     n-to-combine ;; On the next iteration this is the number of longer lists
                     n-to-leave   ;; On the next iteration this is the number of shorter lists
                  )
               )
            ]
      )
   )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define asteroids_plus_lilith '(⚳ ⚴ ⚵ ⚶ ⚷ ⚸))

;; 𝄞   𝄬 ♭ ♯ ♭ & 𝄫 𝄪 𝄲 𝄰 𝄱 & 𝄳


(define (make-image x size color n-syms org-radius)
  (cond 
    [(string? x) (text x size color)]
    [(symbol? x) (text (symbol->string x) size color)]
    [(zero? x)   (text " " size color)]
;;  [(number? x) (text (number->string x) size color)]
    [(number? x) (text (symbol->string (list-ref asteroids_plus_lilith (- x 1))) size color)]
    [(pair? x) (draw-circle-with-evenly-spaced-symbols (* 2 org-radius (sin (/ pi n-syms 2))) size x)]
    [(image? x) x]
  )
)


(define (draw-circle-with-evenly-spaced-symbols radius symsize symlista)
   (let* ([n-syms (length symlista)]
          [angle-delta (/ (+ pi pi) n-syms)]
         )
     (let loop ([angle 0]
                [syms symlista]
                [big-circle (underlay (circle (* 1.2 radius) "solid" "white") (circle radius "outline" "red"))]
               )
           (cond [(null? syms) big-circle]
                 [else (loop (+ angle angle-delta)
                             (rest syms) 
                             (overlay/offset 
                                  (make-image (first syms) symsize "black" n-syms radius)
                                  (* radius (cos angle))
                                  (* radius (sin angle))
                                  big-circle 
                             )
                       )
                 ]
           )
     )
   )
)



(define pentatonic_minor_C4_onward (map (lambda (p) (+ 60 p)) '(0 3 5 7 10 12 15 17)))

(define (ints->midinotes n) (if (zero? n) n (list-ref pentatonic_minor_C4_onward (- n 1))))

;; (build-vector 6 (lambda (n) (permute-A060118 (build-vector 6 add1) 3 n)))

(define (permute-A060118 elems size permrank)
  (let ((p (vector-take elems size)))
    (let unrankA060118 ((r permrank)
                        (i 1)
                       )
          (cond ((zero? r) p)
                (else
                   (let* ((j (add1 i))
                          (m (modulo r j))
                         )
                      (cond ((not (zero? m)) ;; Swap at i and (i-(r mod (i+1)))
                                (let ((org-i (vector-ref p i)))
                                   (vector-set! p i (vector-ref p (- i m)))
                                   (vector-set! p (- i m) org-i)
                                )
                            )
                      )
                      (unrankA060118 (/ (- r m) j) j)
                   )
                )
          )
    )
  )
)


(define (rol lista) (if (null? lista) lista (append (cdr lista) (list (car lista)))))

;; Entä ror?

(define (deeprol lista) (if (not (pair? lista)) lista (rol (map deeprol lista)))) ;; Voiko rol:in laittaa toiseen paikkaan?

(define (get-rotating-closure initial-lista once-every-nth-tick)
  (let ([empty (empty-scene 10 10)]
        [our-private-lista initial-lista]
       )
    (lambda (frametime)
         (cond [(zero? (modulo frametime once-every-nth-tick))
                  (let ([prevnote (ints->midinotes (first our-private-lista))])
                    (midi-turn-off prevnote 127)
                    (set! our-private-lista (deeprol our-private-lista))
                    (midi-turn-on (ints->midinotes (first our-private-lista)) 127)
                  )
               ]
         )
;        (draw-circle-with-evenly-spaced-symbols 200 52 our-private-lista)
         empty
    )
  )
)


(define (start tempo) (rtmidi-open-port out 1) (animate (get-rotating-closure (euclidean_1_0_rhythm 8 5) tempo)))

(define (koke1 n-onsets n-beats pause) (kokeileuc n-onsets n-beats pause ints->midinotes))

(define (koke2 n-onsets n-beats pause note) (kokeileuc n-onsets n-beats pause (lambda (discard) (ints->midinotes note))))


(define (kokeileuc n-onsets n-beats pause int2midinote_fun)
  (let loop ([notes (euclidean_rhythm n-onsets n-beats)]
             [prevnote 0]
            )
    (let ([currentnote (first notes)])
       (cond [(not (zero? currentnote))
                (and (not (zero? prevnote)) (midi-turn-off (int2midinote_fun prevnote) 127))
                (midi-turn-on (int2midinote_fun currentnote) 127)
             ]
       )
       (sleep pause)
       (loop (rol notes) (if (zero? currentnote) prevnote currentnote))
    )
  )
)

(define (polytry) (polyeurytmiaa '((3 8) (5 13) (2 8)) '(60 72 65) 0.125))
(define (polytry2) (polyeurytmiaa '((3 8) (5 13) (6 19)) '(61 72 65) 0.125))

(define (polyeurytmiaa list-of-onsets_beats list-of-notevalues pause)
  (let loop ([note-lists (map (lambda (onsets_beats) (euclidean_rhythm (first onsets_beats) (second onsets_beats))) list-of-onsets_beats)]
             [prevnotes (make-list (length list-of-onsets_beats) 0)] ;; Notes still playing, if any. (zero: not in this circle)
            )
    (let ([new-prevnotes
            (map (lambda (nextnote noteplaying midinote)
                   (cond [(not (zero? nextnote))
                             (and (not (zero? noteplaying)) (midi-turn-off midinote 127))
                             (midi-turn-on midinote 127)
                         ]
                   )
                   (if (zero? nextnote) noteplaying nextnote)
                 )
                 (map first note-lists) ;; Get the next notes to be played (0 for no change)
                 prevnotes
                 list-of-notevalues
            )
          ]
         )
       (sleep pause)
       (loop (map rol note-lists) new-prevnotes)
    )
  )
)



(polytry)

