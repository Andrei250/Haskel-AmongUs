#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)

;; ATENȚIE: Pentru această etapă a temei este necesar să implementați
;;          întâi TDA-ul queue în fișierul queue.rkt.
;; Reveniți la sarcinile din acest fișier după ce ați implementat tipul 
;; queue și ați verificat implementarea folosind checker-ul.


; Structura counter nu se modifică.
; Ceea ce se modifică este implementarea câmpului queue:
; - în loc de listă, acesta va fi o coadă (o structură de tip queue)
; - acest lucru nu este vizibil în definiția structurii counter,
;   ci în implementarea operațiilor acestui tip de date (tipul counter)
(define-struct counter (index tt et queue) #:transparent)


; TODO
; Actualizați funcțiile de mai jos astfel încât ele să folosească
; o structură de tip queue pentru reprezentarea cozii de persoane.
; Elementele cozii continuă să fie perechi (nume . nr_produse).
; Este esențial să respectați "bariera de abstractizare", adică să
; operați cu coada doar folosind interfața acestui TDA:
; - empty-queue
; - queue-empty?
; - enqueue
; - dequeue
; - top
; Obs: Doar câteva funcții vor necesita actualizări.
(define (empty-counter index)           ; testată de checker
  (make-counter index 0 0 empty-queue))

(define (update f counters index)
  (map (λ (e) (if (equal? index (match e [(counter index _ _ _) index])) (f e) e)) counters))

(define tt+
  (λ (minutes)
    (λ (C)
      (match C [(counter index tt et queue) (make-counter index (+ tt minutes) et queue)]))))

(define et+
  (λ (minutes)
    (λ (C)
      (match C [(counter index tt et queue) (make-counter index tt (+ et minutes) queue)]))))

(define (add-to-counter name items)     ; testată de checker
  (λ (C)                                ; nu modificați felul în care funcția își primește argumentele
    (match C [(counter index tt et queue) (if (queue-empty? queue)
                                                (make-counter index (+ tt items) (+ et items) (enqueue (cons name items) queue))
                                                (make-counter index (+ tt items) et (enqueue (cons name items) queue)))])))

(define (get-minimum fct counters)
  (if (null? counters)
      (cons -1 -1)
  (foldl (λ (el pair) (if (< (fct el) (cdr pair))
                          (match-let ([(counter index tt et queue) el]) (cons index (fct el)))
                          pair)) (match-let ([(counter index tt et queue) (car counters)]) (cons index (fct (car counters)))) (cdr counters))))

(define min-tt (λ (counters) (get-minimum (λ (el) (match el [(counter index tt et queue) tt])) counters)))
(define min-et (λ (counters) (get-minimum (λ (el) (match el [(counter index tt et queue) et])) counters)))

(define (remove-first-from-counter C)   ; testată de checker
  (match C [(counter index tt et queue)
            (cond
              ((queue-empty? (dequeue (counter-queue C))) (empty-counter (counter-index C)))
              (else (make-counter (counter-index C) (- (counter-tt C) (counter-et C)) (cdr (top (dequeue (counter-queue C)))) (dequeue (counter-queue C)))))]))


; TODO
; Implementați o funcție care calculează starea unei case după un număr dat de minute.
; Funcția presupune, fără să verifice, că în acest timp nu a ieșit nimeni din coadă, 
; deci va avea efect doar asupra câmpurilor tt și et.
; (cu alte cuvinte, este responsabilitatea utilizatorului să nu apeleze această funcție
; cu minutes > timpul până la ieșirea primului client din coadă)
; Atenție: casele fără clienți nu trebuie să ajungă la timpi negativi!
(define (pass-time-through-counter minutes)
  (λ (C)
         (make-counter (counter-index C) (max 0 (- (counter-tt C) minutes)) (max 0 (- (counter-et C) minutes)) (counter-queue C))))

; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 2, apar modificări în:
; - formatul listei de cereri (parametrul requests)
; - formatul rezultatului funcției (explicat mai jos)
; requests conține 4 tipuri de cereri (3 moștenite din etapa 2 plus una nouă):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă            (ca înainte)
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute       (ca înainte)
;   - (ensure <average>) - cât timp tt-ul mediu al tuturor caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)         (ca înainte)
;   - <x> - trec <x> minute de la ultima cerere, iar starea caselor se actualizează
;           corespunzător (cu efect asupra câmpurilor tt, et, queue)                     (   NOU!   )
; Obs: Au dispărut cererile de tip remove-first, pentru a lăsa loc unui mecanism mai 
; sofisticat de a scoate clienții din coadă (pe măsură ce trece timpul).
; Sistemul trebuie să proceseze cele 4 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele cu tt minim (dintre casele la care au voie)  (ca înainte)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți) (ca înainte)
; - tt-ul mediu (ttmed) se calculează pentru toate casele (și cele fast, și cele slow), 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>       (ca înainte)
; - când timpul prin sistem avansează cu <x> minute, tt-ul, et-ul și queue-ul tuturor 
;   caselor se actualizează pentru a reflecta trecerea timpului; dacă unul sau mai mulți 
;   clienți termină de stat la coadă, ieșirile lor sunt contorizate în ordine cronologică.
; Rezultatul funcției serve va fi o pereche cu punct între:
; - lista sortată cronologic a clienților care au părăsit supermarketul
;   - fiecare element din listă va avea forma (index_casă . nume)
;   - dacă mai mulți clienți ies simultan, sortați-i crescător după indexul casei
; - lista caselor în starea finală (ca rezultatul din etapele 1 și 2)
; Obs: Pentru a contoriza ieșirile din cozi, puteți să lucrați într-o funcție ajutătoare
; (cu un parametru în plus față de funcția serve), pe care serve doar o apelează.

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


(define (pass-time-through-counter-helper minutes)
  (λ (C)
     (if (> minutes (counter-et C))
         (make-counter (counter-index C) 0 0 (counter-queue C))
         ((pass-time-through-counter minutes) C))))

(define (reduce-minimum acc minutes fast-counters slow-counters)
  (cond
    ((= 0 minutes) (list acc fast-counters slow-counters))
    (else (let ((mins (cdr (min-et (filter (λ (e) (not (queue-empty? (counter-queue e)))) (append fast-counters slow-counters))))))
            (cond
              ((= mins -1) (list acc (map (pass-time-through-counter-helper minutes) fast-counters) (map (pass-time-through-counter-helper minutes) slow-counters)))
              ((< minutes mins) (reduce-minimum acc 0 (map (pass-time-through-counter-helper minutes) fast-counters) (map (pass-time-through-counter-helper minutes) slow-counters)))
              (else (let ((lst (foldl (λ(e acum) (append acum (list (cons (counter-index e) (car (top (counter-queue e))))) )) '() (filter (λ (e) (and (= mins (counter-et e)) (not (queue-empty? (counter-queue e))))) (append fast-counters slow-counters)))))
                  (reduce-minimum (append acc lst) (- minutes mins) (map (λ (e) (if (and (= mins (counter-et e)) (not (queue-empty? (counter-queue e))))
                                                                                    (remove-first-from-counter ((pass-time-through-counter-helper mins) e))
                                                                                    ((pass-time-through-counter-helper mins) e)
                                                                                    )) fast-counters)
                                                                    (map (λ (e) (if (and (= mins (counter-et e)) (not (queue-empty? (counter-queue e))))
                                                                                    (remove-first-from-counter ((pass-time-through-counter-helper mins) e))
                                                                                    ((pass-time-through-counter-helper mins) e)
                                                                                    )) slow-counters))))
              )))))
                
  

(define (serve-helper acc requests fast-counters slow-counters)
   (if (null? requests)
      (cons acc (append fast-counters slow-counters))
      (match (car requests)
        [(list 'ensure average) (serve-helper acc (cdr requests) fast-counters (get-new-list-of-counters average fast-counters slow-counters))]
        [(list name n-items)
         (if (> n-items ITEMS)
             (serve-helper acc (cdr requests) fast-counters (update (add-to-counter name n-items) slow-counters (car (min-tt slow-counters))))
             (serve-helper acc (cdr requests) (update (add-to-counter name n-items) fast-counters (car (min-tt (append fast-counters slow-counters)))) (update (add-to-counter name n-items) slow-counters (car (min-tt (append fast-counters slow-counters)))))
             )]
        [(list 'delay index minutes) (serve-helper acc (cdr requests) (update (tt+ minutes) (update (et+ minutes) fast-counters index) index) (update (tt+ minutes) (update (et+ minutes) slow-counters index) index))] 
        [minutes (let ((ans (reduce-minimum '() minutes fast-counters slow-counters)))
                   (serve-helper (append acc (car ans)) (cdr requests) (car (cdr ans)) (car (cdr (cdr ans))))
                   )])))

(define (serve requests fast-counters slow-counters)
  (serve-helper '() requests fast-counters slow-counters))