#lang racket
;;(free-variables '((a b) (λ c ((d c) (e b))))) ⇒ (a b d e)

(define free-variables (λ (exp)
                         (cond [(empty? exp) '()]
                               [(list? (car exp))
                                (cond [(not (member 'λ (car exp)))
                                       (cons (car exp) (free-variables (cadr exp)))]
                                      [else ( free-variables (caddr exp))])]
                         [else (cond [(equal? (car exp) 'λ) '()]
                                     [else (cons (car exp) '())])])))