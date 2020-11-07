#lang racket

(require examples)

;; Estrutura da Lista.
(define-struct link (first rest) #:transparent)

;; Estrutura da frequencia de palavra.
(define-struct freqWord [string num] #:transparent)

;; Definindo vazio.
(define-struct empty () #:transparent)


;; Modelo para lista
#;
(define (fn-para-ldn ldn)
  (cond
    [(empty? ldn) ...]
    [else ... (first ldn)
          ... (fn-para-ldn (rest ldn)) ... ]))

;;------------------Função que monta o array com suas respectivas ocorrencias.-----------------------
(define (verify-occurrence string lst)
  (cond
    [(empty? lst) 0]
    [(string=? string (link-first lst)) (+ 1 (verify-occurrence string (link-rest lst)))]
    [else (verify-occurrence string (link-rest lst))]
    )
  )
;;---------------------------------------------------------------------------------------------------

;;--------------------Função que remove os itens que forem duplicados.-------------------------------
(define (remove-duplicates string lst)
  (cond [(empty? lst) (empty)]
        [else
         (if (string=? string (link-first lst))
             (remove-duplicates string (link-rest lst))
             (link (link-first lst) (remove-duplicates string (link-rest lst))))]))

;;---------------------------------------------------------------------------------------------------

;;------------------------------------ SORT -----------------------------------------------

(define (inserts-ordered frequency-struct lst)
  (cond
    [(empty? lst) (link frequency-struct (empty))]
    [(> (freqWord-num frequency-struct) (freqWord-num (link-first lst))) (link frequency-struct lst)]
    [else (link (link-first lst)
                (inserts-ordered frequency-struct (link-rest lst)))]))

(define (sort lst)
  (cond
    [(empty? lst) (empty)]
    [else (inserts-ordered (link-first lst)
                           (sort (link-rest lst)))]))
;;-----------------------------------------------------------------------------------------------------

;;==== Funcao Contador ======
(define (rank count-number chosen-number lst)
  ;;(print lst)
  (cond
    [(empty? lst) (empty)]
    [(= count-number chosen-number) (link (link-first lst) (empty))]
    [else (link (link-first lst) (rank (add1 count-number) chosen-number (link-rest lst)))]))


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
     
     
(define test (link "Sakura" (link "Naruto" (link "Kakashi" (link "Naruto" (empty))))))
(define test1 (link (freqWord "Naruto" 2) (link (freqWord "Kakashi" 1) (link (freqWord "Sakura" 1) (empty)))))
;;(define test69 (link 1 (link 9 (link 5 (link -2 (link 8 (link 3 (empty))))))))
;;(insere-ordenado (freqWord "Marcao" 0) test1)
;;(insere-ordenado (freqWord "Marcao" 0) (link (freqWord "Marcao" 1) (empty)))
;;(insere-ordenado (freqWord "Marcao" 3) (link (freqWord "Marcao" 1) (empty)))
