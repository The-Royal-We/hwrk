#lang racket
;;(free-variables '((a b) (λ c ((d c) (e b))))) ⇒ (a b d e)
;; If there's a symbol followed by a λ then drop it out
;; If there's a list followed by a λ then drop it out
;; Otherwise just return the list with all of the variables


;;> (cons (car '(λ c ((d c) (e b)))) (cadr '(λ c ((d c) (e b))) ))
;;'(λ . c)
;;> (cons (car '(λ c ((d c) (e b)))) (cadr '(λ (c b) ((d c) (e b))) ))
;;'(λ c b)

(define free-variables (λ (exp)
                         (cond [(empty? exp) '()]
                               [(list? exp)
                                (cond [(member (car exp) '(λ))
                                       (free-variables-a (cdr exp) (cadr exp))]
                                      [else (cons (car exp) (free-variables (cadr exp)))])]
                         [else (cond [(equal? exp 'λ) '()]
                                     [else (cons exp '())])])))

 (define free-variables-a (λ (exp bounded) ()))

(define (flatmap f list)
  apply append (map f list))

(define (flatten x)
    (cond ((null? x) '())
          ((not (pair? x)) (list x))
          (else (append (flatten (car x))
                        (flatten (cdr x))))))

(flatmap (λ (xs) (filter (λ (exp) (not (equal? 'λ exp))) xs)) '((a b) (λ c ((d c) (e b)))) )