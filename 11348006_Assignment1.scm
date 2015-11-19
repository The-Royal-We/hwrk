#lang racket

(define my-list '(1 2 3))
(define alphabet '(a b c d e f g h j k l m n o p q r s t u v w x y z))

(define set-cardinally
  (λ (xs) 
     (cond [(null? xs) 0]
	   [else
	    (+ 1 (set-cardinally (cdr xs)))])))

(define test-set-cardinally
  (equal? 3 (set-cardinally my-list)))

					; A union of two sets results in a set containing elements of the first and second sets 
					; With no duplicated elements

(define set-union
  (λ (s1 s2)
     (cond [(null? s1)s2]
	   [(member (car s1) s2)
	    (set-union (cdr s1) s2)]
	   [else 
	    (cons (car s1) (set-union (cdr s1) s2))])))

(define test-set-union
  (equal? '(a b c d) (set-union '(a b c) '(b c d))))

(define set-intersection
  (lambda (s1 s2)
    (cond ((null? s1) '())
          ((member (car s1) s2)
           (cons (car s1)
                 (set-intersection (cdr s1) s2)))
          (else (set-intersection (cdr s1) s2)))))

(define test-set-intersection
  (equal? '(b c) (set-intersection '(a b c) '(b c d))))

(define set-equal?
  (λ (s1 s2)
     (and (subset? s1 s2)
	  (subset? s2 s1))))

(define subset?
  (lambda (s1 s2)
    (or (null? s1)
        (and (member (car s1) s2)
             (subset? (cdr s1) s2)))))

(define test-set-equal?
  (equal? #t (set-equal? '(a b c) '(b c a))))

(define set-map-join
  (λ (f xs)
     (cond [(null? xs) '()]
	   [else
	    (cons
	     (f (car xs))
	     (set-map-join f (cdr xs)))])))

(define test-map-join
  (equal? '(2 3 4) (set-map-join (λ (e) (+ e 1)) my-list)))

;;(free-variables '((a b) (λ c ((d c) (e b))))) ⇒ (a b d e)
;; If there's a symbol followed by a λ then drop it out
;; If there's a list followed by a λ then drop it out
;; Otherwise just return the list with all of the variables


;;> (cons (car '(λ c ((d c) (e b)))) (cadr '(λ c ((d c) (e b))) ))
;;'(λ . c)
;;> (cons (car '(λ c ((d c) (e b)))) (cadr '(λ (c b) ((d c) (e b))) ))
;;'(λ c b)

(define (free-variables exp)
  (remove-duplicates (flatten (free-variablesp exp))))

(define free-variablesp (λ (exp)
			   (cond [(null? exp) '()]
				 [(symbol? exp) (append (cons exp '()))]
				 [(pair? exp)
				  (cond [(member (car exp) '(λ))
					 (free-variables (deep-filter (λ (expression)
									 (cond [(symbol? (cadr exp))
										(not (equal? (cadr exp) expression))]
									       [else
										(not (member expression (cadr exp)))]))
								      (caddr exp)))]
					[else
					 (cond [(equal? (length exp) 1)
						(cons (car exp) (free-variables (cdr exp)) )]
					       [else (cons (car exp) (free-variables (cadr exp)))])])]
				 [else error "Something went wrong somewhere..."])))

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

;;(free-variables '((a b) (λ c ((d c) (e b)))))
;;(free-variables '((a b) (λ (c e) ((d c) (e b)))))

;; β-reduction
;; This is simple substitution of an expression to a bound variable as such: (λx . x) . y -> y
;; However, we must alpha rename any expressions in a lower bounded context if bound symbols are repeated: ((λ x . (λ xp . xp) x) 2) -> 2
;; Check if lower vaiables are bounded to the same symbol, alpha rename to prime
;;
;; Do it in 2 passes. α-rename then substitution

;; cons (β-reduction (cadar exp)) (cdar exp)

;; (cdar '((λ x (((λ x (x y)) x) (x b))) z))
;; > '(x (((λ x (x y)) x) (x b)))


(define β-reduction
  (λ (exp)
     (cond [(null? exp) '()]
	   [(pair? exp)
	    (cond [(member (car exp) '(λ))
		   (β-reduction)])])))


;; Half-finished. I have it ass backwards.
;; FIX THIS!!
;;(define α-rename (λ (expression bound)
;;                   (cond [(null? bound) '()]
;;                         [(symbol? bound)
;;                          (replace (gensym bound) bound expression)]
;;                         [(pair? bound)
;;                        ;; (println bound)
;;                          (let ([bound-symbol (car bound)]) bound-symbol
;;                          (α-rename (replace (gensym bound-symbol) bound-symbol expression) (cdr bound) ))]
;;                        [else error "something fkd up somewhere..."])))


(define α-rename (λ (expression bound)
                   (cond [(null? expression) '()]
                         [(pair? (car expression))
                          ;;(println (car expression))
                          (α-rename (car expression) bound)]
                         [else
                          (println expression)
                          (cons
                                (replace (gensym bound) bound (car expression))
                                (α-rename (cdr expression) bound) )])))

(define replace
  (λ (new old l)
    (cond [(null? l) '()]
          [atom? (car l)
                 (cond [(eq? (car l) old)
                        (cons new
                              (replace new old (cdr l)))]
                       [else (cons (car l)
                                   (replace new old (cdr l)))])]
          [else (cons (replace new old (car l))
                      (replace new old (cdr l)))])))

;; Do I pass in a list of bound variables? Nahh
;; Do I do a deep α-rename ???


(α-rename '(λ x y) 'y)
(α-rename '(λ x (λ y x y) x) 'y)








