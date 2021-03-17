#lang racket
(require racket/match)

(provide (all-defined-out))

(define ITEMS 5)

;; Actualizăm structura counter cu informația et:
;; Exit time (et) al unei case reprezintă timpul până la ieșirea primului client de la casa respectivă,
;; adică numărul de produse de procesat pentru acest client + întârzierile suferite de casă (dacă există).
;; Ex:
;; la C3 s-au așezat întâi Ana cu 3 produse și apoi Geo cu 7 produse,
;; și C3 a fost întârziată cu 5 minute => et pentru C3 este 3 + 5 = 8 (timpul până când va ieși Ana).


; Redefinim structura counter.
(define-struct counter (index tt et queue) #:transparent)


; TODO
; Actualizați implementarea empty-counter astfel încât să conțină și câmpul et.
(define (empty-counter index)
  (make-counter index 0 0 '()))


; TODO
; Implementați o funcție care aplică o transformare casei cu un anumit index.
; f = funcție unară care se aplică pe un argument de tip casă, counters = listă de case,
; index = indexul casei din counters căreia îi veți aplica transformarea f
; Veți întoarce lista actualizată de case.
; Dacă nu există în counters o casă cu acest index, veți întoarce lista nemodificată.
(define (update f counters index)
  (map (λ (e) (if (equal? index (match e [(counter index _ _ _) index])) (f e) e)) counters))


; TODO
; Memento: tt+ crește tt-ul unei case cu un număr dat de minute.
; Actualizați implementarea tt+ pentru:
; - a ține cont de noua reprezentare a unei case
; - a permite ca operații de tip tt+ să fie pasate ca argument funcției update în cel mai facil mod
; Obs: Pentru compatibilitatea cu primul argument al lui update, trebuie să obținem ușor din tt+ 
; o funcție unară care se aplică pe un argument de tip casă (v. funcții curry).
; Am eliminat parametrii lui tt+ din define-ul de mai jos astfel încât voi să determinați
; cum este cel mai bine ca tt+ să își primească parametrii.
; Din acest motiv checker-ul nu testează separat această funcție, dar asistentul va verifica
; faptul că ați implementat-o conform cerințelor.
(define tt+
  (λ (minutes)
    (λ (C)
      (match C [(counter index tt et queue) (make-counter index (+ tt minutes) et queue)]))))


; TODO
; Implementați o funcție care crește et-ul unei case cu un număr dat de minute.
; Păstrați formatul folosit pentru tt+.
; Checker-ul nu testează separat această funcție.
(define et+
  (λ (minutes)
    (λ (C)
      (match C [(counter index tt et queue) (make-counter index tt (+ et minutes) queue)]))))


; TODO
; Memento: add-to-counter adaugă o persoană (reprezentată prin nume și număr de produse) la o casă. 
; Actualizați implementarea add-to-counter pentru aceleași rațiuni pentru care am modificat tt+.
; Atenție la cum se modifică tt și et!
; Checker-ul nu testează separat această funcție.
(define add-to-counter
  (λ (pair)
    (λ (C)
      (match C [(counter index tt et queue) (if (null? queue)
                                                (make-counter index (+ tt (cdr pair)) (+ et (cdr pair)) (append queue (list pair)))
                                                (make-counter index (+ tt (cdr pair)) et (append queue (list pair))))]))))


; TODO
; Întrucât vom folosi atât min-tt (implementat în etapa 1) cât și min-et (funcție nouă),
; definiți-vă o funcție mai abstractă din care să derive ușor atât min-tt cât și min-et.
; Prin analogie cu min-tt, definim min-et astfel:
; min-et = funcție care primește o listă de case și intoarce o pereche dintre:
; - indexul casei (din listă) care are cel mai mic et
; - et-ul acesteia
; (când mai multe case au același et, este preferată casa cu indexul cel mai mic)
(define (get-minimum fct counters)
  (if (null? counters)
      (cons -1 -1)
  (foldl (λ (el pair) (if (< (fct el) (cdr pair))
                          (match-let ([(counter index tt et queue) el]) (cons index (fct el)))
                          pair)) (match-let ([(counter index tt et queue) (car counters)]) (cons index (fct (car counters)))) (cdr counters))))

(define min-tt (λ (counters) (get-minimum (λ (el) (match el [(counter index tt et queue) tt])) counters))) ; folosind funcția de mai sus
(define min-et (λ (counters) (get-minimum (λ (el) (match el [(counter index tt et queue) et])) counters))) ; folosind funcția de mai sus

; TODO
; Implementați o funcție care scoate prima persoană din coada unei case.
; Funcția presupune, fără să verifice, că există minim o persoană la coada casei C.
; Veți întoarce o nouă structură obținută prin modificarea cozii de așteptare.
; Atenție la cum se modifică tt și et!
; Dacă o casă tocmai a fost părăsită de cineva, înseamnă că ea nu mai are întârzieri.

(define (remove-first-from-counter C)
  (match C [(counter index tt et queue)
            (cond
              ((null? (cdr (counter-queue C))) (empty-counter (counter-index C)))
              (else (make-counter (counter-index C) (foldl (λ (e acc) (+ acc (cdr e))) 0 (cdr (counter-queue C))) (cdr (car (cdr (counter-queue C)))) (cdr (counter-queue C)))))]))

; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 1, funcția serve operează cu următoarele modificări:
; - nu mai avem doar 4 case, ci:
;   - fast-counters (o listă de case pentru maxim ITEMS produse)
;   - slow-counters (o listă de case fără restricții)
;   (Sugestie: folosiți funcția update pentru a obține comportamentul pe liste de case)
; - requests conține 4 tipuri de cereri (cele 2 din etapa anterioară plus 2 noi):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute
;   - (remove-first) - cea mai avansată persoană părăsește casa la care se află
;   - (ensure <average>) - cât timp tt-ul mediu al tuturor caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)
; Sistemul trebuie să proceseze cele 4 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele cu tt minim (dintre casele la care au voie)
;   (ca înainte, dar acum folosiți fast-counters și slow-counters)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți)
;   (puțin diferit față de etapa 1, și acum folosiți fast-counters și slow-counters)
; - persoana care părăsește supermarketul este persoana fruntașă la casa cu et-ul minim
;   (dintre casele care au clienți); dacă nicio casă nu are clienți, cererea e ignorată
; - tt-ul mediu (ttmed) se calculează pentru toate casele (și cele fast, și cele slow), 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>
; Considerați că indecșii caselor încep de la 1 și sunt mereu în ordine.
; Ex:
; fast-counters conține casele 1-2, slow-counters conține casele 3-15
; => la nevoie veți adăuga întâi casa 16, apoi casa 17, etc.
; (puteți determina matematic de câte case noi este nevoie sau
;  puteți adăuga recursiv una câte una până când condiția este îndeplinită)

(define (get-avg counters)
  (if (null? counters)
      0
      (/ (foldl (λ (e acc) (+ acc (counter-tt e))) 0 counters) (length counters))))

(define (last-element counters)
  (if (null? (cdr counters))
      (+ 1 (counter-index (car counters)))
      (last-element (cdr counters))))

(define (get-last-index fast-counters slow-counters)
  (cond
    ((and (null? fast-counters) (null? slow-counters)) 1)
    ((null? slow-counters) (last-element fast-counters))
    (else (last-element slow-counters))))

(define (get-new-list-of-counters limit fast-counters slow-counters)
  (if (<= (get-avg (append fast-counters slow-counters)) limit)
         slow-counters
         (get-new-list-of-counters limit fast-counters (append slow-counters (list (empty-counter (get-last-index fast-counters slow-counters)))))))

(define (serve requests fast-counters slow-counters)
  (if (null? requests)
      (append fast-counters slow-counters)
      (match (car requests)
        [(list 'ensure average) (serve (cdr requests) fast-counters (get-new-list-of-counters average fast-counters slow-counters))]
        [(list name n-items)
         (if (> n-items ITEMS)
             (serve (cdr requests) fast-counters (update (add-to-counter (cons name n-items)) slow-counters (car (min-tt slow-counters))))
             (serve (cdr requests) (update (add-to-counter (cons name n-items)) fast-counters (car (min-tt (append fast-counters slow-counters)))) (update (add-to-counter (cons name n-items)) slow-counters (car (min-tt (append fast-counters slow-counters)))))
             )]
        [(list 'delay index minutes) (serve (cdr requests) (update (tt+ minutes) (update (et+ minutes) fast-counters index) index) (update (tt+ minutes) (update (et+ minutes) slow-counters index) index))]
        [(list 'remove-first)  (serve (cdr requests) (update (λ (e) (remove-first-from-counter e)) fast-counters (car (min-et (filter (λ (e) (> (length (counter-queue e)) 0)) (append fast-counters slow-counters))))) (update (λ (e) (remove-first-from-counter e)) slow-counters (car (min-et (filter (λ (e) (> (length (counter-queue e)) 0)) (append fast-counters slow-counters))))))]  
        )))


(define C1 (empty-counter 1))
(define C2 (empty-counter 2))
(define C3 (empty-counter 3))
(define C4 (empty-counter 4))
(define C5 (make-counter 5 12 8 '((remus . 6) (vivi . 4))))

