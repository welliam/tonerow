(load "tonerow.scm")

(define (every p lst)
  (or (null? lst)
      (and (p (car lst))
           (every p (cdr lst)))))

(define (sublist? lst1 lst2)
  (every (cut member <> lst2) lst1))

(define scales '())

(define-syntax define-scale
  (syntax-rules (:)
    ((_ name scale)
     (define-scale-f `(name ,scale)))
    ((_ name : notes ...)
     (define-scale name '(notes ...)))))

(define (define-scale-f scale-entry)
  (set! scales (cons scale-entry scales)))

(define-syntax scale-synonym
  (syntax-rules ()
    ((_ defined synonyms ...)
     (for-each (lambda (s)
                 (define-scale-f (list s (lookup-scale 'defined))))
               '(synonyms ...)))))

(define (lookup-scale name)
  (let ((lookup (assq name scales)))
    (if lookup (car (cdr lookup)) #f)))

(define-syntax sref
  ; intended for quick interactive use
  (syntax-rules ()
    ((_ x) (lookup-scale 'x))))

(define (find-scales p)
  (filter (cut* p (second <>)) scales))

(define (find-scales-containing subscale)
  (let ((s (if (symbol? subscale)
               (lookup-scale subscale)
               subscale)))
    (find-scales (cut sublist? s <>))))

; --- VARIOUS ---------------------------
(define-scale major-pentatonic : C D E G A)
(define-scale minor-pentatonic : C Eb F G Bb)
(define-scale blues            : C Eb F Gb G Bb)
(define-scale prometheus       : C D E Gb A Bb)
(define-scale pelog            : C Db Eb Gb G Ab Bb)
(define-scale slendro          : C D F G A)
(scale-synonym slendro yo)
(define-scale iwato            : C Db F Gb Bb)
(define-scale hirajoushi       : C E Gb G B)
(define-scale insen            : C Db F G Bb)

; --- CONSTELLATIONS --------------------
(define-scale chromatic  12-tone-row:c)
(define-scale whole-tone : C D E Gb Ab Bb)
(define-scale tritone    : C Gb)
(define-scale augmented  : C E Ab)
(define-scale diminished : C Eb Gb A)

; --- MOLTS -----------------------------
(scale-synonym whole-tone molt1)
(define-scale molt2       (interpolate/c (sref diminished) 1))
(define-scale secret-mode (interpolate/c (sref augmented) -1))
(define-scale molt3       (interpolate/c (sref secret-mode) -1))
(define-scale molt4       : C Db D F Gb G Ab B)
(define-scale molt5       : C Db F Gb G B)
(define-scale molt6       : C D E F Gb Ab Bb B)
(define-scale molt7       : C Db D Eb F Gb G Ab A B)

; --- TONAL SCALES --------------------------------
(define-scale major          : C D E F G A B)
(define-scale natural-minor  : C D Eb F G Ab Bb)
(define-scale harmonic-minor : C D Eb F G Ab B)
(define-scale melodic-minor  : C D Eb F G A B)

; --- HANDLING MODES ------------------------------
(define-syntax define-modes-from
  (syntax-rules ()
    ((_ parent-scale names ...)
     (let loop ((modes (find-modes/c (lookup-scale 'parent-scale)))
                (mode-names '(names ...)))
       (cond
        ((null? mode-names) #t)
        ((eq? (car mode-names) 'skip)
         (loop (cdr modes) (cdr mode-names)))
        (else (define-scale-f (list (car mode-names) (car modes)))
              (loop (cdr modes) (cdr mode-names))))))))

; --- MAJOR MODES ---------------------------------
(define-modes-from major
  ionian dorian phrygian lydian mixolydian aeolian locrian)

; --- MINOR MODES ---------------------------------
(define-modes-from melodic-minor
  skip dorian-b2 lydian-augmented lydian-dominant
  mixolydian-b6 half-diminished altered-dominant)

(define-modes-from harmonic-minor
  skip locrian-nat6 ionian-#5 ukranian-minor
  phrygian-dominant lydian-#2 altered-diminished)

(define-scale double-harmonic : C Db E F G Ab B)
(define-modes-from double-harmonic
  lydian-#2-#6 phrygian-bb7-b4 hungarian-minor locrian-nat6-nat3
  ionian-#5-#2 locrian-bb3-bb7)

(set! scales (reverse scales)) ; we tend to define more common scales sooner
