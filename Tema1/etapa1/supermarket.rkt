#lang racket
(require racket/match)

(provide (all-defined-out))

(define ITEMS 5)

;; C1, C2, C3, C4 sunt case într-un supermarket.
;; C1 acceptă doar clienți care au cumparat maxim ITEMS produse (ITEMS este definit mai sus).
;; C2 - C4 nu au restricții.
;; Considerăm că procesarea fiecărui produs la casă durează un minut.
;; Casele pot suferi întarzieri (delay).
;; La un moment dat, la fiecare casă există 0 sau mai mulți clienți care stau la coadă.
;; Timpul total (tt) al unei case reprezintă timpul de procesare al celor aflați la coadă,
;; adică numărul de produse cumpărate de ei + întârzierile suferite de casa respectivă (dacă există).
;; Ex:
;; la C3 sunt Ana cu 3 produse și Geo cu 7 produse,
;; și C3 nu are întârzieri => tt pentru C3 este 10.


; Definim o structură care descrie o casă prin:
; - index (de la 1 la 4)
; - tt (timpul total descris mai sus)
; - queue (coada cu persoanele care așteaptă)
(define-struct counter (index tt queue) #:transparent)

; TODO
; Implementați o functie care intoarce o structură counter goală.
; tt este 0 si coada este vidă.
; Obs: la definirea structurii counter se creează automat o funcție make-counter pentru a construi date de acest tip
(define (empty-counter index)
  (make-counter index 0 '()))

; TODO
; Implementați o funcție care crește tt-ul unei case cu un număr dat de minute.
(define (tt+ C minutes)
  (match C
    [(counter index tt queue)
     (make-counter index (+ tt minutes) queue)]))


; TODO
; Implementați o funcție care primește o listă de case și intoarce o pereche dintre:
; - indexul casei (din listă) care are cel mai mic tt
; - tt-ul acesteia
; Obs: când mai multe case au același tt, este preferată casa cu indexul cel mai mic
(define (check-one-element L)
  (cond
    ((null? L) #f)
    ((null? (cdr L)) #t)
    (else #f)))

(define (minimum_time S1 pr)
  (if (<= (match S1 [(counter index tt queue) tt]) (cdr pr))
      (match S1 [(counter index tt _) (cons index tt)])
      pr))

(define (min-tt counters)
  (cond
    ((null? counter) (cons null null))
    ((check-one-element counters) (match (car counters) [(counter index tt _) (cons index tt)]))
    (else (minimum_time (car counters) (min-tt (cdr counters))))))

; TODO
; Implementați o funcție care adaugă o persoană la o casă.
; C = casa, name = numele persoanei, n-items = numărul de produse cumpărate
; Veți întoarce o nouă structură obținută prin așezarea perechii (name . n-items)
; la sfârșitul cozii de așteptare.

(define (add-element-to-counter-queue C el)
  (match C
    [(counter index tt queue)
     (make-counter index tt (append queue (list el)))]))

(define (add-to-counter C name n-items)
  (cond
    ((equal? (match C [(counter index _ _ ) index]) 1)
     (if (> n-items ITEMS)
         C
         (tt+ (add-element-to-counter-queue C (cons name n-items)) n-items)))
    (else (tt+ (add-element-to-counter-queue C (cons name n-items)) n-items))))


; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; requests = listă de cereri care pot fi de 2 tipuri:
; - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă
; - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute
; C1, C2, C3, C4 = structuri corespunzătoare celor 4 case
; Sistemul trebuie să proceseze aceste cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele cu tt minim (dintre casele la care au voie)
; - când o casă suferă o întârziere, tt-ul ei crește

(define (return-new-structure C ind minutes)
  (match C [(counter index tt queue)
            (if (equal? index ind)
                (tt+ C minutes)
                C)]))

(define (find-by-index structures ind)
  (cond
    ((null? structures) '())
    ((match (car structures) [(counter index _ _) (equal? index ind)]) (car structures))
     (else (find-by-index (cdr structures) ind))))

(define (get-answer-list L1 L2 ans name n-items)
  (cond
    ((null? L1) ans)
    ((match (car L1) [(counter index _ _) (equal? index (car (min-tt L2)))])  (get-answer-list (cdr L1) L2 (append ans (list (add-to-counter (find-by-index L2 (car (min-tt L2))) name n-items))) name n-items))
    (else (get-answer-list (cdr L1) L2 (append ans (list (car L1))) name n-items))))

(define (serve requests C1 C2 C3 C4)

  ; puteți să vă definiți aici funcții ajutătoare (define în define)
  ; - avantaj: aveți acces la variabilele requests, C1, C2, C3, C4 fără a le retrimite ca parametri
  ; puteți de asemenea să vă definiți funcțiile ajutătoare în exteriorul lui "serve"
  ; - avantaj: puteți să vă testați fiecare funcție imediat ce ați implementat-o

  (if (null? requests)
      (list C1 C2 C3 C4)
      (match (car requests)
        [(list 'delay index minutes) (serve (cdr requests) (return-new-structure C1 index minutes) (return-new-structure C2 index minutes) (return-new-structure C3 index minutes) (return-new-structure C4 index minutes))]
        [(list name n-items)
         (if (match C1 [(counter index tt queue) (equal? index (car (min-tt (list C1 C2 C3 C4))))])
             (if (equal? (match C1 [(counter index tt queue) tt]) (match (add-to-counter C1 name n-items) [(counter index tt queue) tt]))
                 (serve (cdr requests) (car (get-answer-list (list C1 C2 C3 C4) (list C2 C3 C4) '() name n-items))
                                   (car  (cdr (get-answer-list (list C1 C2 C3 C4) (list C2 C3 C4) '() name n-items)))
                                   (car (cdr (cdr (get-answer-list (list C1 C2 C3 C4) (list C2 C3 C4) '() name n-items))))
                                   (car (cdr (cdr (cdr (get-answer-list (list C1 C2 C3 C4) (list C2 C3 C4) '() name n-items))))))
                 (serve (cdr requests) (add-to-counter C1 name n-items) C2 C3 C4))
             (serve (cdr requests) (car (get-answer-list (list C1 C2 C3 C4) (list C1 C2 C3 C4) '() name n-items))
                                   (car  (cdr (get-answer-list (list C1 C2 C3 C4) (list C1 C2 C3 C4) '() name n-items)))
                                   (car (cdr (cdr (get-answer-list (list C1 C2 C3 C4) (list C1 C2 C3 C4) '() name n-items))))
                                   (car (cdr (cdr (cdr (get-answer-list (list C1 C2 C3 C4) (list C1 C2 C3 C4) '() name n-items)))))))])))

