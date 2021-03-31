#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)


; TODO
; Aveți libertatea să vă structurați programul cum doriți (dar cu restricțiile
; de mai jos), astfel încât funcția serve să funcționeze conform specificației.
; 
; Restricții (impuse de checker):
; - trebuie să existe în continuare funcția (empty-counter index)
; - cozile de la case trebuie implementate folosind noul TDA queue

(define-struct counter (index tt et queue opened) #:transparent)

(define (empty-counter index)
  (make-counter index 0 0 empty-queue 1))


(define (update f counters index)
  (map (λ (e) (if (equal? index (match e [(counter index _ _ _ _) index])) (f e) e)) counters))

(define close
  (λ (C)
    (make-counter (counter-index C) (counter-tt C) (counter-et C) (counter-queue C) 0)))

(define tt+
  (λ (minutes)
    (λ (C)
      (if (= 1 (counter-opened C))
          (make-counter (counter-index C) (+ (counter-tt C) minutes) (counter-et C) (counter-queue C) (counter-opened C))
          C))))
      

(define et+
  (λ (minutes)
    (λ (C)
      (if (= 1 (counter-opened C))
          (make-counter (counter-index C) (counter-tt C) (+ (counter-et C) minutes) (counter-queue C) (counter-opened C))
          C))))

(define (add-to-counter name items)   
  (λ (C)                              
    (match C [(counter index tt et queue opened) (if (queue-empty? queue)
                                                (make-counter index (+ tt items) (+ et items) (enqueue (cons name items) queue) opened)
                                                (make-counter index (+ tt items) et (enqueue (cons name items) queue) opened))])))

(define (get-minimum fct counters)
  (if (null? counters)
      (cons -1 -1)
  (foldl (λ (el pair) (if (< (fct el) (cdr pair))
                          (match-let ([(counter index tt et queue opened) el]) (cons index (fct el)))
                          pair)) (match-let ([(counter index tt et queue opened) (car counters)]) (cons index (fct (car counters)))) (cdr counters))))

(define min-tt (λ (counters) (get-minimum (λ (el) (match el [(counter index tt et queue opened) tt])) counters)))
(define min-et (λ (counters) (get-minimum (λ (el) (match el [(counter index tt et queue opened) et])) counters)))

(define (remove-first-from-counter C)  
  (match C [(counter index tt et queue opened)
            (cond
              ((queue-empty? (dequeue (counter-queue C))) (if ( = 1 (counter-opened C)) (empty-counter (counter-index C)) (close (empty-counter (counter-index C)))))
              (else (make-counter (counter-index C) (- (counter-tt C) (counter-et C)) (cdr (top (dequeue (counter-queue C)))) (dequeue (counter-queue C)) opened)))]))


(define (pass-time-through-counter minutes)
  (λ (C)
         (make-counter (counter-index C) (max 0 (- (counter-tt C) minutes)) (max 0 (- (counter-et C) minutes)) (counter-queue C) (counter-opened C))))

  
; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 3, apare un nou tip de cerere, așadar
; requests conține 5 tipuri de cereri (cele moștenite din etapa 3 plus una nouă):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă              (ca înainte)
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute         (ca înainte)
;   - (ensure <average>) - cât timp tt-ul mediu al caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)           (ca înainte)
;   - <x> - trec <x> minute de la ultima cerere, iar starea caselor se actualizează
;           corespunzător (cu efect asupra câmpurilor tt, et, queue)                       (ca înainte)
;   - (close <index>) - casa index este închisă                                            (   NOU!   )
; Sistemul trebuie să proceseze cele 5 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele DESCHISE cu tt minim; nu se va întâmpla
;   niciodată ca o persoană să nu poată fi distribuită la nicio casă                       (mică modificare)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți);
;   nu aplicați vreun tratament special caselor închise                                    (ca înainte)
; - tt-ul mediu (ttmed) se calculează pentru toate casele DESCHISE, 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>         (mică modificare)
; - când timpul prin sistem avansează cu <x> minute, tt-ul, et-ul și queue-ul tuturor 
;   caselor se actualizează pentru a reflecta trecerea timpului; dacă unul sau mai mulți 
;   clienți termină de stat la coadă, ieșirile lor sunt contorizate în ordine cronologică. (ca înainte)
; - când o casă se închide, ea nu mai primește clienți noi; clienții care erau deja acolo
;   avansează normal, până la ieșirea din supermarket                                    
; Rezultatul funcției serve va fi o pereche cu punct între:
; - lista sortată cronologic a clienților care au părăsit supermarketul:
;   - fiecare element din listă va avea forma (index_casă . nume)
;   - dacă mai mulți clienți ies simultan, sortați-i crescător după indexul casei
; - lista cozilor (de la case DESCHISE sau ÎNCHISE) care încă au clienți:
;   - fiecare element va avea forma (index_casă . coadă) (coada este de tip queue)
;   - lista este sortată după indexul casei
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
  (if (<= (get-avg (filter (λ (e) (= 1 (counter-opened e))) (append fast-counters slow-counters))) limit)
         slow-counters
         (get-new-list-of-counters limit fast-counters (append slow-counters (list (empty-counter (get-last-index fast-counters slow-counters)))))))


(define (pass-time-through-counter-helper minutes)
  (λ (C)
     (if (> minutes (counter-et C))
         (make-counter (counter-index C) 0 0 (counter-queue C) (counter-opened C))
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
      (cons acc (foldl (λ (e acm) (if (not (queue-empty? (counter-queue e))) (append acm (list (cons (counter-index e) (counter-queue e)))) acm)) '() (append fast-counters slow-counters)))
      ;(cons (cons acc (foldl (λ (e acm) (append acm (list (cons (counter-index e) (counter-queue e))))) '() (append fast-counters slow-counters))) (append fast-counters slow-counters))
      
      (match (car requests)
        [(list 'ensure average) (serve-helper acc (cdr requests) fast-counters (get-new-list-of-counters average fast-counters slow-counters))]
        [(list 'close index) (serve-helper acc (cdr requests) (update close fast-counters index) (update close slow-counters index))]
        [(list name n-items)
         (if (> n-items ITEMS)
             (serve-helper acc (cdr requests) fast-counters (update (add-to-counter name n-items) slow-counters (car (min-tt (filter (λ (e) (= 1 (counter-opened e))) slow-counters)))))
             (serve-helper acc (cdr requests) (update (add-to-counter name n-items) fast-counters (car (min-tt (filter (λ (e) (= 1 (counter-opened e))) (append fast-counters slow-counters))))) (update (add-to-counter name n-items) slow-counters (car (min-tt (filter (λ (e) (= 1 (counter-opened e))) (append fast-counters slow-counters))))))
             )]
        [(list 'delay index minutes) (serve-helper acc (cdr requests) (update (tt+ minutes) (update (et+ minutes) fast-counters index) index) (update (tt+ minutes) (update (et+ minutes) slow-counters index) index))] 
        [minutes (let ((ans (reduce-minimum '() minutes fast-counters slow-counters)))
                   (serve-helper (append acc (car ans)) (cdr requests) (car (cdr ans)) (car (cdr (cdr ans))))
                   )])))

(define (serve requests fast-counters slow-counters)
  (serve-helper '() requests fast-counters slow-counters))
