#lang racket

(require "etapa2.rkt")

(provide (all-defined-out))

; TODO 1
; După modelul funcției stable-match?, implementați funcția
; get-unstable-couples care primește o listă de logodne
; engagements, o listă de preferințe masculine mpref și o 
; listă de preferințe feminine wpref, și întoarce lista
; tuturor cuplurilor instabile din engagements.
; Precizări (aspecte care se garantează, nu trebuie verificate):
; - fiecare cuplu din lista engagements are pe prima poziție
;   o femeie
; Nu este permisă recursivitatea pe stivă.
; Nu sunt permise alte funcții ajutătoare decât
; better-match-exists? și funcțiile de manipulare a listelor de
; preferințe definite în etapele anterioare.
; Nu este permisă apelarea multiplă a aceleiași funcții pe
; aceleași argumente.
; Folosiți una sau mai multe dintre expresiile let, let*, letrec,
; named let pentru a vă putea conforma acestor restricții.
(define (get-unstable-couples engagements mpref wpref)
  (define rev_eng (map (λ (e) (cons (cdr e) (car e))) engagements))
  (let iter ([iter_engagements engagements]
             [unstable_matches '()])
    (if (null? iter_engagements)
        unstable_matches
        (let* ([crt_eng (car iter_engagements)]
               [crtw (car crt_eng)]
               [crtm (cdr crt_eng)]
               [crtm_list (get-pref-list mpref crtm)]
               [crtw_list (get-pref-list wpref crtw)])
          (if (or (better-match-exists? crtm crtw crtm_list wpref engagements)
                  (better-match-exists? crtw crtm crtw_list mpref rev_eng))
              (iter (cdr iter_engagements) (cons crt_eng unstable_matches))
              (iter (cdr iter_engagements) unstable_matches))))))


; TODO 2
; Implementați funcția engage care primește o listă free-men
; de bărbați încă nelogodiți, o listă de logodne parțiale 
; engagements (unde fiecare cuplu are pe prima poziție o femeie),
; o listă de preferințe masculine mpref și o listă de preferințe 
; feminine wpref, și întoarce o listă completă de logodne stabile,
; obținută conform algoritmului Gale-Shapley:
; - cât timp există un bărbat m încă nelogodit
;   - w = prima femeie din preferințele lui m pe care m nu a cerut-o încă
;   - dacă w este nelogodită, adaugă perechea (w, m) la engagements
;   - dacă w este logodită cu m'
;     - dacă w îl preferă pe m lui m'
;       - m' devine liber
;       - actualizează partenerul lui w la m în engagements
;     - altfel, repetă procesul cu următoarea femeie din lista lui m
; Folosiți named let pentru orice proces recursiv ajutător (deci nu
; veți defini funcții ajutătoare recursive).
; Folosiți let și/sau let* pentru a evita calcule duplicate.
(define (engage free-men engagements mpref wpref)
  (let iter ([iter_men free-men]
             [all_engagements engagements])
    (if (null? iter_men)
        all_engagements
        (let ([m (car iter_men)])
          (let iter2 ([pref-list-m (get-pref-list mpref m)])
            (if (null? pref-list-m)
                (iter (cdr iter_men) all_engagements)
                (let* ([w (car pref-list-m)]
                       [partner (get-partner all_engagements w)]
                       [pref-list-w (get-pref-list wpref w)])
                  (cond [(not partner) (iter (cdr iter_men)
                                             (cons (cons w m) all_engagements))]
                        [(preferable? pref-list-w m partner) (iter (append (cdr iter_men) (list partner))
                                                                   (update-engagements all_engagements w m))]
                        [else (iter2 (cdr pref-list-m))]))))))))


; TODO 3
; Implementați funcția gale-shapley care este un wrapper pentru
; algoritmul implementat de funcția engage. Funcția gale-shapley
; primește doar o listă de preferințe masculine mpref și o listă
; de preferințe feminine wpref și calculează o listă completă de
; logodne stabile conform acestor preferințe.
(define (gale-shapley mpref wpref) (engage (get-men mpref) '() mpref wpref))


; TODO 4
; Implementați funcția get-couple-members care primește o listă
; de perechi cu punct și întoarce o listă simplă cu toate elementele 
; care apar în perechi.
; Folosiți funcționale, fără recursivitate explicită.
(define (get-couple-members pair-list)
  (foldl (λ (e acc) (cons (cdr e) (cons (car e) acc))) '() pair-list))

