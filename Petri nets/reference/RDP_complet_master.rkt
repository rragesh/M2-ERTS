#lang scheme

(require scheme/control)
(require scheme/gui/base)
(require srfi/43)
(require htdp/matrix)

;-----------------------------------------------------------------------------------------------------------
; Structure of the Petri Nets : Example with an unbounded Petri net
(define P '(P1 P2 P3 P4 P5 P6 P7 P8 P9 P10));
(define T '(T1 T2 T3 T4 T5 T6 T7 T8));
(define °P '((P1) (P2 T1) (P3 T2) (P4 T4) (P5 T5) (P6 T3) (P7 T6) (P8) (P9 T7) (P10)));
(define P° '((P1 T1) (P2 T2) (P3 T4 T5 T8) (P4 T3) (P5 T3 T6) (P6) (P7) (P8 T6 T7) (P9) (P10 T3 T6)));
(define °T '((T1 P1) (T2 P2) (T3 P4 P5 P10) (T4 P3) (T5 P3) (T6 P5 P8 P10) (T7 P8) (T8 P3)))
(define T° '((T1 P2) (T2 P3) (T3 P6) (T4 P4) (T5 P5) (T6 P7) (T7 P9) (T8)))
(define Arc '((P1 T1 1)  (T1 P2 1)  (P2 T2 1)   (T2 P3 1)   (P4 T3 1)   (P5 T3 1)   (P10 T3 1)   (T3 P6 1)  (P3 T4 1)
              (T4 P4 1)   (P3 T5 1)   (T5 P5 1)   (P5 T6 1)   (P8 T6 1)   (P10 T6 1)   (T6 P7 1)   (P8 T7 1)   (T7 P9 1)   (P3 T8 1))) 
(define M_0 (list->vector '(1 1 0 0 0 0 0 0 0 1)))
(define Pm '((P1 1) (P2 1) (P3 0) (P4 0) (P5 0) (P6 0) (P7 0) (P8 0) (P9 0) (P10 1)));
;-----------------------------------------------------------------------------------------------------------
(define W (for/hash ([e Arc]) (values (drop-right e 1) (last e))))

(define (readW key)
  (cond 
    [(hash-has-key? W key) (hash-ref W key)]
    [else 0]))

(define (print E) 
  (for ([elt E])
    (display elt))) 

(define (membre elt E)
  (cond
    [(empty? E) #f]
    [(equal? (car E) elt) #t]
    [else (membre elt (cdr E))]))

(define (membre2 elt E)
  (cond
    [(empty? E) #f]
    [(equal? (car E) elt) #t]
    [(list? (car E)) (membre2 elt (car E))]
    [else (membre2 elt (cdr E))]))

(define (conjoints elt liste) 
  (cond 
    [(empty? liste) empty]
    [(if (equal? (caar liste) elt) (cdr (car liste)) (conjoints elt (cdr liste)))]))

(define (° pt)
  (if (membre pt T)(conjoints pt °T)(conjoints pt °P)))

(define (◇ t) 
  (if (membre t T)(conjoints t T°)(conjoints t P°)))

(define (°° pt)
  (cond 
    [(list? (° pt)) (for/list ([e (° pt)]) (° e))]
    [else (° (° pt))])) 

(define (◇◇ pt) 
  (cond 
    [(list? (◇ pt)) (for/list ([e (◇ pt)]) (◇ e))]
    [else (◇ (◇ pt))])) 

(define (conflict P)
  (cond 
    [(empty? P) empty]
    [(> (length (◇ (car P))) 1) (cons (cons (car P) (◇ (car P))) (conflict (cdr P)))]
    [else (conflict (cdr P))]))

(define conflicts (conflict P))

(define (conflict? pt)
  (cond
    [(empty? conflicts) #f]
    [else (membre2 pt conflicts)]))

(define (liberateur) null) 

(define GDM_s (make-hash))

;-------------------------------------------------------------------------------------------------------------------
(define (Write clef table valeur)
      (if (hash-set! table clef valeur) #t #f))

;-------------------------------------------------------------------------------------------------------------------
(define (list-pos e l)
  (cond 
    [(not (membre  e l)) #f]
    [(equal? e (car l)) 0]
    [else (+ 1 (list-pos e (cdr l)))]))
  


;-------------------------------------------------------------------------------------------------------------------
(define (mark p M) ; M(p)
  (cond 
    [(not (membre p P)) #f]
    [(vector? M) (vector-ref M (list-pos p P))]  
    [else #f]))
;-------------------------------------------------------------------------------------------------------------------
(define (>>= M1 M2)  ; determine if for all i in dim(M2) M1[i] >= M2[i]
  (cond 
    [(equal? (vector-length M2) 0) #t]
    [else (for/and ([v1 M1] [v2 M2]) (v1 . >= . v2) )]))

;-------------------------------------------------------------------------------------------------------------------
(define (>> M1 M2)  ; determine if for all i in dim(M2) M1[i] >= M2[i] and it exists at least an index j such as M1[j] > M2[j]
   (cond 
    [((vector-ref M1 0) . > . (vector-ref M2 0)) ((vector-drop M1 1) . >>= . (vector-drop M2 1))]
    [((vector-ref M1 0) . = . (vector-ref M2 0)) ((vector-drop M1 1) . >> . (vector-drop M2 1))]
    [else #f]))

;-------------------------------------------------------------------------------------------------------------------
(define (->? M t) ; t is enabled in M ? for all p in °t, M(p) >=  W(p,t)
  (let ([°t (° t)]) 
    (cond                           ; t est sensibilise par M si 
      [(not (membre t T)) #f]
      [(empty? °t) #t] 
      [else (for/and ([p °t]) (>= (mark p M) (hash-ref W (list p t))))])))


;-------------------------------------------------------------------------------------------------------------------
(define (enabled M) ;set of enabled transitions in M
  (filter (lambda(t) (M . ->? . t)) T))

; test (enabled M_0)

;-------------------------------------------------------------------------------------------------------------------
; M' = M + pre - post
(define (-> M t) ; 
       (list->vector(for/list ([p P]) 
                  (+ (- (mark p M) (readW (list p t))) (readW (list t p)))))) 
; test (-> M_0 'T1)

;-------------------------------------------------------------------------------------------------------------------
(define (new-marking? M t) 
  (cond 
    [(empty? t) (not (hash-has-key? GDM_s M))]
    [else (if (M . ->? . t) (not (hash-has-key? GDM_s (M . -> . t))) #f)]))
; test (new-marking? M_0 'T1)

;-------------------------------------------------------------------------------------------------------------------
(define (>_Once M table) ;return true if it exists Mi in table such as M >> Mi 
  (for/first ([(Mi v ) table] #:when (M . >> . Mi  ))#t)) 
; 

;-------------------------------------------------------------------------------------------------------------------
; computation of marking graph
(define (GDM_it M firable)
  (cond 
    [(empty? firable) empty]
    [else (for/list ([t firable]); ITERATION
       (let ([Mi (M . -> . t)])
         (begin 
           (if (hash-has-key? GDM_s Mi) 
             ; IF : The marking is present in the marking graph Then we add the transition found 
             (Write Mi GDM_s (cons (cons t (car (hash-ref GDM_s Mi))) (cdr (hash-ref GDM_s Mi))))
             
             ; Else : we're dealing with a new marking. 
             (if (Mi . >_Once . GDM_s) 
                 ; IF : The new marking is > to a marking already found (Once) in this case we stop
                 (begin (display "\n The Petri net is unbounded \n") (abort #f))    
                 ; ELSE : We have a new marking different from the existing ones: we insert it and iterate in depth
                 (let ([outputs (enabled Mi)])
                   (begin 
                     (Write Mi GDM_s (list (list t) outputs))  ; insertion
                     (GDM_it Mi outputs))) ))        ; in-depth relaunch first
         )))  ; in width after
     ]))

;;-------------------------------------------------------------------------------------------------------------------
; Initiation of the computation of marking graph
(define (GDM)
  (let ([firable_M0 (enabled M_0)])
    (begin
      (hash-set! GDM_s M_0 (list empty firable_M0))
      (GDM_it M_0 firable_M0)
     )))


;;-------------------------------------------------------------------------------------------------------------------
; Computing time
(define (test)
  (let ([deb (current-inexact-milliseconds)])    
             (begin 
               (GDM) 
               (let ([fin (current-inexact-milliseconds)]) 
                 (display (- fin deb))))))
