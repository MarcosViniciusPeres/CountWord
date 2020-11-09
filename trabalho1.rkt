#lang racket

;; Marcos Vinicius Peres | RA:94594
(require examples)

;; Lista(String), Número -> Lista(Com as k strings mais frequentes e o número de vezes que elas aparecem)

;; Dada a entrada uma lista de string e um numero, mostrar um rank dessa lista,
;; partindo da strings mais frequentes encontrada nessa lista.

;; Estrutura da Lista.
(define-struct link (first rest) #:transparent)

;; Estrutura da frequencia de palavra.
(define-struct freqWord [string num] #:transparent)

;; Definindo vazio.
(define-struct empty () #:transparent)


;;------------------Retorna o numero de vezes que ocorreu a string na lista-----------------------

(examples
 (check-equal? (verify-occurrence "Naruto" (empty)) 0)
 (check-equal? (verify-occurrence "Naruto" (link "Naruto" (empty))) 1)
 (check-equal? (verify-occurrence "Naruto" (link "Naruto" (link "Naruto" (empty)))) 2)
 )

#;
(define (verify-occurrence string lst)
  (cond
    [(empty? lst) ...]
    [(string=? string (link-first lst)) ...]
    [else ...]
    )
  )

(define (verify-occurrence string lst)
  (cond
    [(empty? lst) 0]
    [(string=? string (link-first lst)) (+ 1 (verify-occurrence string (link-rest lst)))]
    [else (verify-occurrence string (link-rest lst))]
    )
  )
;;---------------------------------------------------------------------------------------------------

;;--------------------Remove o item que for igual a string.-------------------------------

(examples
 (check-equal? (remove-duplicates "Naruto" (empty)) (empty))
 (check-equal? (remove-duplicates "Naruto" (link "Naruto" (empty))) (empty))
 (check-equal? (remove-duplicates "Naruto" (link "Naruto" (link "Sakura" (empty)))) (link "Sakura" (empty)))
 (check-equal? (remove-duplicates "Naruto" (link "Sakura" (link "Kakashi" (link "Naruto" (empty))))) (link "Sakura" (link "Kakashi" (empty))))
 )

#;
(define (remove-duplicates string lst)
  (cond [(empty? lst) ...]
        [else
         (if (string=? string (link-first lst))
             ...
             ...)]))

(define (remove-duplicates string lst)
  (cond [(empty? lst) (empty)]
        [else
         (if (string=? string (link-first lst))
             (remove-duplicates string (link-rest lst))
             (link (link-first lst) (remove-duplicates string (link-rest lst))))]))

;;---------------------------------------------------------------------------------------------------

;;------------------- Função que ordenará nosso array já na estrutura freqWord ----------------------
(examples
 (check-equal? (sort (empty)) (empty))
 (check-equal? (sort (link (freqWord "Naruto" 1) (empty))) (link (freqWord "Naruto" 1) (empty)))
 (check-equal? (sort (link (freqWord "Naruto" 1) (link (freqWord "Kakashi" 3) (empty))))
               (link (freqWord "Kakashi" 3) (link (freqWord "Naruto" 1) (empty))))
 (check-equal? (sort (link (freqWord "Naruto" 1) (link (freqWord "Kakashi" 2) (link (freqWord "Sakura" 3) (empty)))))
               (link (freqWord "Sakura" 3) (link (freqWord "Kakashi" 2) (link (freqWord "Naruto" 1) (empty)))))

 (check-equal? (inserts-ordered (freqWord "Naruto" 1) (empty)) (link (freqWord "Naruto" 1) (empty)))
 (check-equal? (inserts-ordered (freqWord "Naruto" 2) (link (freqWord "Kakashi" 1) (empty)))
               (link (freqWord "Naruto" 2) (link (freqWord "Kakashi" 1) (empty))))
 (check-equal? (inserts-ordered (freqWord "Naruto" 1) (link (freqWord "Kakashi" 2) (empty)))
               (link (freqWord "Kakashi" 2) (link (freqWord "Naruto" 1) (empty))))
 )

#;
(define (sort lst)
  (cond
    [(empty? lst) ...]
    [else ...]))
#;
(define (inserts-ordered frequency-struct lst)
  (cond
    [(empty? lst) ...]
    [(> (freqWord-num frequency-struct) (freqWord-num (link-first lst))) ...]
    [else ...]))

(define (sort lst)
  (cond
    [(empty? lst) (empty)]
    [else (inserts-ordered (link-first lst)
                           (sort (link-rest lst)))]))

(define (inserts-ordered frequency-struct lst)
  (cond
    [(empty? lst) (link frequency-struct (empty))]
    [(> (freqWord-num frequency-struct) (freqWord-num (link-first lst))) (link frequency-struct lst)]
    [else (link (link-first lst)
                (inserts-ordered frequency-struct (link-rest lst)))]))
;;-----------------------------------------------------------------------------------------------------

;;------------------------------------ Função que irá montar o rank -----------------------------------

(examples
 (check-equal? (rank 1 1 (empty)) (empty))
 (check-equal? (rank 1 1 (link (freqWord "Naruto" 1) (empty))) (link (freqWord "Naruto" 1) (empty)))
 (check-equal? (rank 1 1 (link (freqWord "Naruto" 5) (link (freqWord "Kakashi" 3) (empty)))) (link (freqWord "Naruto" 5) (empty)))
 (check-equal? (rank 1 2 (link (freqWord "Naruto" 5) (link (freqWord "Kakashi" 3) (empty))))
                (link (freqWord "Naruto" 5) (link (freqWord "Kakashi" 3) (empty))))
 (check-equal? (rank 1 100 (link (freqWord "Naruto" 5) (link (freqWord "Kakashi" 3) (empty))))
                (link (freqWord "Naruto" 5) (link (freqWord "Kakashi" 3) (empty))))
 )

#;
(define (rank count-number chosen-number lst)
  (cond
    [(empty? lst) ...]
    [(= count-number chosen-number) ...]
    [else ...]))

(define (rank count-number chosen-number lst)
  (cond
    [(empty? lst) (empty)]
    [(= count-number chosen-number) (link (link-first lst) (empty))]
    [else (link (link-first lst) (rank (add1 count-number) chosen-number (link-rest lst)))]))

;;-----------------------------------------------------------------------------------------------------


;;------------------------------------ Função Main ----------------------------------------------------

(examples
 (check-equal? (count-word (empty) 1) (empty))
 (check-exn exn:fail? (thunk (count-word (link "Naruto" (empty)))))
 (check-equal? (count-word (link "Naruto" (empty)) 1) (link (freqWord "Naruto" 1) (empty)))
 (check-equal? (count-word (link "Naruto" (empty)) 2) (link (freqWord "Naruto" 1) (empty)))
 (check-equal? (count-word (link "Naruto" (link "Naruto" (link "Kakashi" (empty)))) 1)(link (freqWord "Naruto" 2) (empty)))
 (check-equal? (count-word (link "Naruto" (link "Naruto" (link "Kakashi" (empty)))) 2)
               (link (freqWord "Naruto" 2) (link (freqWord "Kakashi" 1) (empty))))
 (check-equal? (count-word (link "Naruto" (link "Naruto" (link "Kakashi" (empty)))) 3)
               (link (freqWord "Naruto" 2) (link (freqWord "Kakashi" 1) (empty))))
 (check-equal? (count-word (link "Naruto" (link "Kakashi" (link "Sakura" (link "Kakashi" (link "Naruto" (link "Naruto" (empty))))))) 3)
                           (link (freqWord "Naruto" 3) (link (freqWord "Kakashi" 2) (link (freqWord "Sakura" 1) (empty)))))
 )

#;
(define (count-word lst number)
  (cond
    [(empty? lst) ...]
    [(<= number 0) ...]
    [else 
     (let* ([occurrence ...]
            [new-list ...]
            [word-frequency ...]
            [formatted-list ...]
            [sort-list ...])
             ... )]))

(define (count-word lst number)
  (cond
    [(empty? lst) (empty)]
    [(<= number 0) (error "You can't choose a number equal to or less than zero.")]
    [else 
     (let* ([occurrence (verify-occurrence (link-first lst) lst)]
            [new-list (remove-duplicates (link-first lst) lst)]
            [word-frequency (freqWord (link-first lst) occurrence)]
            [formatted-list (link word-frequency (count-word new-list number))]
            [sort-list (sort formatted-list)])
             (rank 1 number sort-list) )]))
          