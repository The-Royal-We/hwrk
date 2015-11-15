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
                         (cond [(null? exp) '()]
                               [(pair? exp)
                                (cond [(member (car exp) '(λ))
                                       (append (deep-filter (λ (expression) (not (equal? (cadr exp) expression))) (cdr exp)) (cdr exp)) (free-variables (cdr exp)) ]
                                       ;;(filter (bound? (cadr exp) (cdr exp)))]
                                      [else (append (car exp) (free-variables (cadr exp)))])]
                         [else (append '(exp) '())])))

(define (deep-filter f lst)
  (cond ((null? lst)
         '())
        ((atom? (car lst))
         (if (f (car lst))
             (cons (car lst) (deep-filter f (cdr lst)))
             (deep-filter f (cdr lst))))
        (else
         (filter (compose not null?)
                 (cons (deep-filter f (car lst)) 
                       (deep-filter f (cdr lst)))))))                                      
                                 

(define atom?
  (λ (x)
    (and (not (pair? x)) (not (null? x)))))


(define (flatmap f list)
  apply append (map f list))

(define (flatten x)
    (cond ((null? x) '())
          ((not (pair? x)) (list x))
          (else (append (flatten (car x))
                        (flatten (cdr x))))))

(define (deep-map f l)
  (let deep ((x l))
    (cond [(null? x) x]
          [(pair? x) (map deep x)]
          [else (f x)])))

;;(flatmap (λ (xs) (filter (λ (exp) (not (equal? 'λ exp))) xs)) '((a b) (λ c ((d c) (e b)))) )#
(free-variables '((a b) (λ c ((d c) (e b)))))