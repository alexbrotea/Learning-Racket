#lang racket

; ignorați următoarele linii de cod...
(define show-defaults 2) ; câte exerciții la care s-au întors rezultate default să fie arătate detaliat
(define prepend #f) (define nopoints #t) (define name-ex '(testul testele trecut exercițiul)) ; variante: '(exercițiul exercițiile rezolvat exercițiul) sau '(testul testele trecut exercițiul) sau '(task taskurile rezolvat capitolul)
(define default-results `(#f 0 () your-code-here ,empty-stream ,(range 2 12) (your-code-here your-code-here your-code-here your-code-here your-code-here))) (define (default-result r) (set! default-results (cons r default-results))) (define : 'separator) (define punct 'string) (define puncte 'string) (define BONUS 'string) (define exerciții 'string)
(define total 0) (define all '()) (define n-ex 0) (define p-ex 0) (define n-exercs -1) (define default-returns '()) (define (ex n sep p . s) (set! n-ex n) (set! p-ex p) (set! all (cons (list n p) all))) (define exercițiul ex) (define (sunt n s) (set! n-exercs n)) (define s-a string-append)
(define (p . L) (map (λ (e) (display e) (when (> (string-length (format "~a" e)) 0) (display " "))) L) (newline)) (define (p-n-ex) (format "[~a]" (if nopoints (string-join (list (symbol->string (cadddr name-ex)) (number->string n-ex) "/" (number->string n-exercs))) n-ex)))
(define (epart ep% pfix full) (if (< (caddr ep%) 1) (s-a pfix (if full "" (s-a (symbol->string (car name-ex)) " ")) (if (and nopoints (not full)) "" (number->string n-ex)) (symbol->string (cadr ep%))) (if (and nopoints (not full)) "" (s-a pfix (if full "" (s-a (symbol->string (car name-ex)) " ")) (number->string n-ex)))))
(define (whengood ep%) (let [(pts (* p-ex (caddr ep%)))] (and (if prepend (printf "+~v: " pts) (printf "~a[OK] " (p-n-ex))) (if nopoints (p (epart ep% "" #f) "rezolvat") (p (epart ep% "" #f) "rezolvat: +" pts (if (= pts 1) 'punct 'puncte))) (set! total (+ total pts)))))
(define (whenbad ep% gvn expcd msg) (and (when (member gvn default-results) (set! default-returns (cons (epart ep% "" #t) default-returns))) (when (or (not (member gvn default-results)) (<= (length default-returns) show-defaults)) (bad-res ep% gvn expcd msg))))
(define (bad-res ep% gvn expcd msg) (p (if prepend "+0.0:" (format "~a[--]" (p-n-ex))) (epart ep% "la " #f) 'rezultatul (format "~v" gvn) msg (if (equal? expcd '////) "" (format "~v" expcd))))
(define (check-conds e gvn conds) (or (null? conds) (let ([r ((car conds) gvn)]) (if (eq? r #t) (check-conds e gvn (cdr conds)) (whenbad e gvn '//// (or r "nu îndeplinește condiția"))))))
(define (check% part per given main-test expected . conds) (let* ([e (list n-ex part per)] [p? (pair? (cdr main-test))] [p (if p? (car main-test) identity)] [t ((if p? cadr car) main-test)] [m ((if p? cddr cdr) main-test)]) (when (eq? #t (check-conds e given conds)) (if (t (p given) expected) (whengood e) (whenbad e (p given) expected m)))))
(define (check given main-test expected . conds) (apply check% '- 1 given main-test expected conds)) (define the cons)
(define is (cons equal? "diferă de cel așteptat"))
(define in (cons member "nu se află printre variantele așteptate"))
(define same-set-as (cons (λ (x y) (apply equal? (map list->seteqv (list x y)))) "nu este aceeași mulțime cu"))
(define same-elements-as (cons (λ (x y) (and (apply = (map length (list x y))) ((car same-set-as) x y))) "nu sunt aceleași rezultate cu"))
(define (sumar) (when (and (not (null? default-returns)) (< show-defaults (length default-returns))) (p "... rezultatul implicit dat la" (cadr name-ex) (reverse default-returns))) (when (not nopoints) (p 'total: total 'puncte)))
(define (mark-helper) (printf "---~nEx  puncte    Total până aici~n") (foldr (λ (e-p t) (p (car e-p) ': (cadr e-p) "puncte. total 1 -" (car e-p) ': (+ t (cadr e-p))) (+ t (cadr e-p))) 0 all) (newline))
; ...până aici.

;; Funcții și valori predefinite pe stream-uri:
;; * stream-cons
;; * stream-first
;; * stream-rest
;; * empty-stream
;; * stream-empty?
;; * stream-map, stream-filter

;; Funcții utile:
;; primește un flux și obține o listă cu primele n elemente din flux
(define (stream-take s n)
  (cond ((zero? n) '())
        ((stream-empty? s) '())
        (else (cons (stream-first s)
                    (stream-take (stream-rest s) (- n 1))))))

; verifică dacă fluxul s are lungimea cel puțin n
(define (stream-check-length s n)
  (cond ((zero? n) #t)
        ((stream-empty? s) #f)
        (else (stream-check-length (stream-rest s) (sub1 n)))))

;; fluxul numerelor naturale
(define (naturals-from n)
  (let loop ((seed n))
    (stream-cons seed (loop (add1 seed)))))

(define naturals (naturals-from 0))

;; fluxul numerelor prime
(define (sieve s)
  (let ((p (stream-first s)))
    (stream-cons p (sieve (stream-filter
                           (λ (n) (not (zero? (modulo n p))))
                           (stream-rest s))))))

(define primes (sieve (naturals-from 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(sunt 9 exerciții)

(exercițiul 1 : 1 punct)
;; funcția list->stream, care creează un flux din elementele unei liste
(define (list->stream L)
  (if (null? L)
      empty-stream
      (stream-cons (car L) (list->stream (cdr L)))))

(check% 'a 1/4 (list? (list->stream '(2 4 6 8 10))) is #f) ;; rezultatul întors este un flux, nu o listă
(check% 'b 1/4 (null? (list->stream '())) is #f) ;; rezultatul întors este un flux (gol), nu o listă (goală)
(check% 'c 1/4 (stream-take (list->stream '(2 4 6 8 10)) 10) is '(2 4 6 8 10))
(check% 'd 1/4 (stream-take (list->stream '(1 2 3 4)) 3) is '(1 2 3))

(exercițiul 2 : 1 punct)
;; funcția stream-zip-with, care primește o funcție binară și 2 fluxuri și funcționează
;; ca un (map f s1 s2) (stream-map nu merge decât cu funcții unare, nu ca map pe liste)
;; stream-zip-with va funcționa și pe fluxuri de lungime diferită, aplicând funcția
;; atât timp cât este posibil
;; ex: (stream-zip-with * naturals naturals) => stream-ul pătratelor numerelor naturale
(define (stream-zip-with f s1 s2)
  (if (or (stream-empty? s1) (stream-empty? s2))
      empty-stream
      (stream-cons (f (stream-first s1) (stream-first s2))
                   (stream-zip-with f (stream-rest s1) (stream-rest s2)))))

(check% 'a 1/2 (stream-take (stream-zip-with + naturals (stream-rest naturals)) 10) is '(1 3 5 7 9 11 13 15 17 19))
(check% 'b 1/2 (stream-take (stream-zip-with list (list->stream '(1 2)) naturals) 3) is '((1 0) (2 1)))

(exercițiul 3 : 1 punct)
;; fluxul care alternează la infinit elementele a și b: a, b, a, b, ...
(define (repeat a b)
   empty-stream)

(check% 'a 1/2 (stream-take (repeat 'hello 'world) 5) is '(hello world hello world hello))
(check% 'b 1/2 (stream-take (repeat '(1 2) 3) 4) is '((1 2) 3 (1 2) 3))

(exercițiul 4 : 2 puncte)
;; fluxul cu elemente din seria Leibniz: 1, -1/3, 1/5, -1/7, ...
;; Se cere implementarea acestuia prin două metode:
;; - construind fluxul explicit, prin generarea fiecărui element
;; - construind fluxul implicit, prin prelucrarea (fără recursivitate explicită) altui/altor flux/fluxuri (cum ar fi naturals)
(define leibniz-series-1 ;; explicit
   empty-stream)

(define leibniz-series-2 ;; implicit
   empty-stream)

(check% 'a 1/2 (stream-take leibniz-series-1 10) is '(1 -1/3 1/5 -1/7 1/9 -1/11 1/13 -1/15 1/17 -1/19))
(check% 'b 1/2 (stream-take leibniz-series-2 10) is '(1 -1/3 1/5 -1/7 1/9 -1/11 1/13 -1/15 1/17 -1/19))

(exercițiul 5 : 2 puncte)
;; fluxul x, y = (f1 x), z = (f2 y), t = (f1 z), u = (f2 t) .... (se dau x, f1 și f2)
;; Se cere o definiție implicită a fluxului (fără a genera recursiv element cu element).
;;
;; Sugestie:
;;         f1              f2                  f1                     f2   ....
;;          x          (f1 x)         (f2 (f1 x))       (f1 (f2 (f1 x)))   ....
;; ----------------------------------------------------------------------------
;;  ?  (f1 x)     (f2 (f1 x))    (f1 (f2 (f1 x)))       .......................
(define (play x f1 f2)
   empty-stream)

(check% 'a 1/2 (stream-take (play 1 sqr add1) 10) is '(1 1 2 4 5 25 26 676 677 458329))
(check% 'b 1/2 (stream-take (play 3 sub1 sqr) 10) is '(3 2 4 3 9 8 64 63 3969 3968))

(exercițiul 6 : 2 puncte)
;; Folosiți funcția de mai sus pentru a afla cine câștigă jocul următor:
;; - 2 jucători, 1 și 2, pleacă de la un număr natural n > 0
;; - cei 2 alternează mutări, 1 mută primul
;; - o mutare a lui 1 constă în a scădea 1 din n-ul curent, dacă n e multiplu de 3, sau a scădea 3, altfel
;; - o mutare a lui 2 constă în a scădea 2 din n-ul curent, dacă n e multiplu de 4, sau a scădea 1, altfel
;; - câștigă cel care ajunge primul la n <= 0
;;
;; Restricții:
;; - NU folosiți recursivitate explicită, ci funcționale pe fluxuri pentru a combina
;;   fluxul generat de funcția play cu informația despre care jucător este la mutare
;; - NU interesează o soluție matematică, ci una care folosește funcția de mai sus!
(define (winner n)
  'your-code-here)

(check% 'a 1/4 (winner 3) is 1)
(check% 'b 1/4 (winner 55) is 2)
(check% 'c 1/4 (winner 7) is 1)
(check% 'd 1/4 (winner 17) is 2)

(exercițiul 7 : 1 punct)
;; funcția not-prime?, care testează că un număr n nu este prim, verificând dacă n se împarte la
;; vreun număr prim generat de ciurul lui Eratostene (care este implementat la începutul
;; laboratorului, și este reținut în variabila primes)
;;
;; Restricții:
;; - NU se acceptă soluții care încearcă și divizori care nu sunt primi!
(define (not-prime? n)
  'your-code-here)

(check (filter not-prime? (range 2 12)) is '(4 6 8 9 10))

(exercițiul 8 : 1 punct)
;; funcția rotations, care generează un flux cu toate rotațiile unui număr n
;; Atenție, fluxul este finit!
;; Pentru eficiență, folosiți funcții pe stringuri (dar la final întoarceți numere):
;; number->string, string->number, string-append, string-length, substring
;; Obs:
;; (substring str start end) întoarce caracterele din str de la poziția start
;;                           până la poziția end-1
;; (substring str start) întoarce caracterele din str de la poziția start până la final
;; ex: (substring "Apple" 1 3) => "pp"
(define (rotations n)
  empty-stream)

(check% 'a 1/4 (rotations 2)
               (the stream->list same-elements-as) '(2)
               (λ (r) (or (not (stream-empty? r)) "trebuie să fie nevid"))
               (λ (r) (or (stream-empty? (stream-rest r)) "trebuie să aibă un singur element")))
(check% 'b 1/4 (rotations 23)
               (the stream->list same-elements-as) '(23 32)
               (λ (r) (or (stream-check-length r 2) "trebuie să aibă minim 2 elemente"))
               (λ (r) (or (stream-empty? (stream-rest (stream-rest r))) "trebuie să aibă maxim 2 elemente")))
(check% 'c 1/2 (stream-take (rotations 7199) 4) same-elements-as '(1997 7199 9719 9971))

(exercițiul 9 : 1 punct)
;; funcția circular-prime?, care verifică dacă un număr n este circular prim, adică
;; dacă toate rotațiile lui n sunt numere prime
;;
;; Restricții:
;; - NU se vor folosi:
;;   - operații pe liste (trebuie lucrat exclusiv cu fluxuri)
;;   - recursivitate explicită
;; Important:
;; - Explicați ce se întâmplă la testul a dacă folosim operații pe liste.
(define (circular-prime? n)
  'your-code-here)

(check% 'a 1/3 (circular-prime? 3710000000000000) is #f)
(check% 'b 1/3 (map circular-prime? (range 2 7)) is '(#t #t #f #t #f))
(check% 'c 1/3 (map circular-prime? (range 11 17)) is '(#t #f #t #f #f #f))


(sumar)
