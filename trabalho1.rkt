#lang racket

(require examples)

(define-struct freqWord [string num] #:transparent)

;; Modelo para lista
;;(define (fn-para-ldn ldn)
;;  (cond
;;    [(empty? ldn) ...]
;;    [else ... (first ldn)
;;          ... (fn-para-ldn (rest ldn)) ... ]))

;; Função que monta o array com suas respectivas ocorrencias.
(define (verify-occurrence string lst)
  (cond
    [(empty? lst) 0]
    [(string=? string (first lst)) (+ 1(verify-occurrence string (rest lst)))]
    [else (verify-occurrence string (rest lst))]
    )
  )

(define (count-word lst)
  (cond
    [(empty? lst) empty]
    [else (cons (freqWord (first lst) (verify-occurrence (first lst) lst)) (count-word (rest lst)))]))

(define test (cons "Marcao"(cons "Marcao"(cons "Marcao" empty))))
