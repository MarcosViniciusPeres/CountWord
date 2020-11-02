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

;; Função que monta o array com suas respectivas ocorrencias.
(define (verify-occurrence string lst)
  (cond
    [(empty? lst) 0]
    [(string=? string (link-first lst)) (+ 1 (verify-occurrence string (link-rest lst)))]
    [else (verify-occurrence string (link-rest lst))]
    )
  )

;;Função que remove os itens que forem duplicados.
(define (remove-duplicates string lst)
  (cond [(empty? lst) (empty)]
        [else
         (if (string=? string (link-first lst))
             (remove-duplicates string (link-rest lst))
             (link (link-first lst) (remove-duplicates string (link-rest lst))))]))

;; Função que irá ordenar o nosso array
#;
(define (quicksort lst)
  (if (empty? lst)
      (empty)
      (let ([pivo (freqWord-num (link-first lst))]
            [resto (link-rest lst)])
        (append (quicksort (filter (curryr < pivo) resto))
                (link (link-first lst))
                (quicksort (filter (curryr >= pivo) resto))))))


(define (count-word lst)
  (cond
    [(empty? lst) (empty)]
    [else 
     (let* ([occurrence (verify-occurrence (link-first lst) lst)]
            [new-list (remove-duplicates (link-first lst) lst)]
            [word-frequency (freqWord (link-first lst) occurrence)]
            [formatted-list (link word-frequency (count-word new-list))])
            formatted-list)]))


;;------------------------------------ INSERTION SORT -----------------------------------------------

(define (inserts-ordered n lst)
  (cond
    [(empty? lst) (link n (empty))]
    [(> (freqWord-num n) (freqWord-num (link-first lst))) (link n lst)]
    [else (link (link-first lst)
                (inserts-ordered n (link-rest lst)))]))


(define (sort lst)
  (cond
    [(empty? lst) (empty)]
    [else (inserts-ordered (link-first lst)
                           (sort (link-rest lst)))]))

;;(define test69 (link 1 (link 9 (link 5 (link -2 (link 8 (link 3 (empty))))))))

;;(define test (link "Sakura" (link "Naruto" (link "Kakashi" (link "Naruto" (empty))))))
(define test1 (link (freqWord "Marcao" 1) (link (freqWord "Marcao" 2) (link (freqWord "Marcao" 3) (empty)))))

;;(insere-ordenado (freqWord "Marcao" 0) test1)
;;(insere-ordenado (freqWord "Marcao" 0) (link (freqWord "Marcao" 1) (empty)))
;;(insere-ordenado (freqWord "Marcao" 3) (link (freqWord "Marcao" 1) (empty)))
