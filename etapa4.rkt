#lang racket
(require "suffix-tree-stream.rkt")
(require "collection.rkt")

(provide (all-defined-out))

;; Vom prelua toate funcțiile din etapele 1-3 (exceptând
;; longest-common-substring, care nu beneficiază de
;; reprezentarea ca flux întrucât parcurge tot arborele)
;; și le vom adapta la noua reprezentare a unui ST.
;;
;; Pentru că un ST este construit pornind de la o colecție
;; de sufixe și pentru că ne dorim să nu calculăm toate
;; sufixele decât dacă este nevoie, vom modifica toate
;; funcțiile care prelucrau liste de sufixe pentru a
;; prelucra fluxuri de sufixe.
;;
;; Obs: fără această modificare a listelor de sufixe în
;; fluxuri de sufixe, și presupunând că am manipulat
;; arborii de sufixe doar prin interfața definită în
;; fișierul suffix-tree (respectând astfel bariera de
;; abstractizare), ar trebui să alterăm doar funcția
;; suffixes->st care este practic un constructor pentru
;; tipul ST.
;; Din cauza transformării listelor de sufixe în fluxuri,
;; avem mult mai multe implementări de modificat.
;; Puteam evita acest lucru? Da, utilizând conceptul de
;; colecție de sufixe de la început (în loc să presupunem
;; că ele vor fi prelucrate ca liste). În loc de cons,
;; car, cdr, map, filter, etc. am fi folosit de fiecare
;; dată collection-cons, collection-first, ... etc. -
;; aceste funcții fiind definite într-o bibliotecă
;; inițială ca fiind echivalentele lor pe liste, și
;; redefinite ulterior în stream-cons, stream-first,
;; ... etc. Operatorii pe colecții de sufixe ar fi
;; folosit, desigur, doar funcții de tip collection-.
;;
;; Am ales să nu procedăm astfel pentru că ar fi provocat
;; confuzie la momentul respectiv (când chiar operatorii
;; pe liste erau o noutate) și pentru a vă da ocazia să
;; faceți singuri acest "re-design".

; TODO
; Copiați din etapele anterioare implementările funcțiilor
; de mai jos și modificați-le astfel:
; - Toate funcțiile care lucrează cu liste de sufixe vor
;   lucra cu un nou tip de date Collection, ai cărui
;   constructori și operatori vor fi definiți de voi
;   în fișierul collection.rkt.
; - Pentru toate funcțiile, trebuie să vă asigurați că
;   este respectată bariera de abstractizare (atât în
;   cazul tipului ST cât și în cazul tipului Collection).
; Obs: cu cât mai multe funcții rămân nemodificate, cu atât
; este mai bine (înseamnă că design-ul inițial a fost bun).

(define (longest-common-prefix w1 w2)
  (define (longest-common-prefix-function w1 w2 pr)
    (if (or (null? w1) (null? w2))
        (list (reverse pr) w1 w2)
        (if (equal? (car w1) (car w2))
            (longest-common-prefix-function (cdr w1) (cdr w2) (cons (car w1) pr))
            (list (reverse pr) w1 w2))))
  (longest-common-prefix-function w1 w2 '()))

; am schimbat, în numele funcției, cuvântul list în
; cuvântul collection
(define (longest-common-prefix-of-collection words)
  (if (collection-empty? (collection-rest words))
      (collection-first words)
      (let* ([first-word (collection-first words)]
             [second-word (collection-first (collection-rest words))]
             [rest-words (collection-rest (collection-rest words))])
        (longest-common-prefix-of-collection
         (collection-cons (car (longest-common-prefix first-word second-word)) rest-words)))))

(define (match-pattern-with-label st pattern)
  (if (not (get-ch-branch st (car pattern)))
      (collection #f '())
      (let* ([branch (get-ch-branch st (car pattern))]
             [label (get-branch-label branch)]
             [prefix (collection-first (longest-common-prefix label pattern))])
        (cond
          [(equal? prefix pattern) #t]
          [(equal? prefix label)
           (collection label (drop pattern (length prefix)) (get-branch-subtree branch))]
          [else (collection #f prefix)]))))

(define (st-has-pattern? st pattern)
  (let* ([func-return (match-pattern-with-label st pattern)])
    (cond
      [(equal? func-return #t) #t]
      [(not (collection-first func-return)) #f]
      [else
       (st-has-pattern? (collection-first (collection-rest (collection-rest func-return)))
                        (collection-first (collection-rest func-return)))])))

(define (get-suffixes text)
  (if (null? (cdr text)) (collection text) (collection-cons text (get-suffixes (cdr text)))))

(define (get-ch-words words ch)
  (let* ([new-words (collection-filter (lambda (word) (not (null? word))) words)])
    (collection-filter (lambda (word) (equal? (car word) ch)) new-words)))

(define (ast-func suffixes)
  (cons (list (car (collection-first suffixes))) (collection-map cdr suffixes)))

(define (drop-prefix lst prefix)
  (if (null? prefix) lst (drop-prefix (cdr lst) (cdr prefix))))

(define (cst-func suffixes)
  (let* ([prefix (longest-common-prefix-of-collection suffixes)])
    (cons prefix (collection-map (lambda (x) (drop-prefix x prefix)) suffixes))))

; considerați că și parametrul alphabet este un flux
; (desigur, și suffixes este un flux, fiind o colecție
; de sufixe)
(define (suffixes->st labeling-func suffixes alphabet)
  (collection-filter
   (lambda (lst) (not (null? lst)))
   (collection-map
    (lambda (ch)
      (let* ([ch-words (get-ch-words suffixes ch)]
             [func-return (if (collection-empty? ch-words) #f (labeling-func ch-words))])
        (if func-return
            (cons (car func-return) (suffixes->st labeling-func (cdr func-return) alphabet))
            '())))
    alphabet)))

; nu uitați să convertiți alfabetul într-un flux
(define (text->st labeling-func)
  (lambda (text)
    (let* ([new-text (reverse (cons #\$ (reverse text)))])
      (suffixes->st labeling-func
                    (get-suffixes new-text)
                    (list->collection (sort (remove-duplicates new-text) char<?))))))

(define (list->collection L)
  (if (null? L) empty-collection (collection-cons (car L) (list->collection (cdr L)))))

(define (text->ast text)
  ((text->st ast-func) text))

(define (text->cst text)
  ((text->st cst-func) text))

; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.
(define (substring? text pattern)
  (st-has-pattern? (text->ast text) pattern))

; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.
(define (repeated-substring-of-given-length text len)
  (define (process-branch iter branch substr st)
    (if (null? branch)
        #f
        (let* ([subtree (get-branch-subtree branch)]
               [label (get-branch-label branch)]
               [current-substr (append substr label)])
          (cond
            [(iter subtree current-substr) (iter subtree current-substr)]
            [else (iter (other-branches st) substr)]))))
  (let iter ([st (text->cst text)] [substr '()])
    (cond
      [(st-empty? st) #f]
      [(> len (length substr)) (process-branch iter (first-branch st) substr st)]
      [else (take substr len)])))
