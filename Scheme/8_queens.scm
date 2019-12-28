(use-modules (srfi srfi-1))

(set! *random-state* (random-state-from-platform))

(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(define (comb n k)
  (if (> k n)
      0
      (/ (factorial n)
	 (* (factorial (- n k))
	    (factorial k)))))
  
(define (enumerate-interval low high)
  (define (enumerate-interval-iter high interval-list)
    (if (< high low)
        interval-list
	(enumerate-interval-iter (- high 1)
				 (cons high interval-list))))
  (enumerate-interval-iter high '()))


(define (same-row-col? p1 p2)
  (or
   (=
    (car p1)
    (car p2))
   (=
    (cdr p1)
    (cdr p2))))


(define (same-diagonal? p1 p2)
  (let ((dx (abs
	     (- (car p1)
		(car p2))))
	(dy (abs
	     (- (cdr p1)
		(cdr p2)))))
    (= dx dy)))


(define (sum list-of-integers)
  (define (sum-iter accumulated-value list-of-integers)
    (if (null? list-of-integers)
	accumulated-value
	(sum-iter
	 (+ accumulated-value
	    (car list-of-integers))
	 (cdr list-of-integers))))
  (sum-iter 0 list-of-integers))
	

(define (chromosome-fn minimum maximum chromosome-size)
  (define (generate-random-interval-iter length interval-list)
    (if (= length 0)
	interval-list
	(generate-random-interval-iter
	 (- length 1)
	 (cons (+ minimum (random maximum)) interval-list))))
  (generate-random-interval-iter
   chromosome-size
   '()))


(define (create-population population-size minimum maximum chromosome-size)
  (define (create-population-list-iter length population-list)
    (if (= length 0)
	population-list
	(create-population-list-iter
	 (- length 1)
	 (cons
	  (chromosome-fn minimum maximum chromosome-size)
	  population-list))))
  (create-population-list-iter
   population-size
   '()))


(define (fitness-fn chromosome)
  (define (iter list-of-coordinates result)
    (if (null? (cdr list-of-coordinates))
	result
	(let ((queen-a-coordinates
	       (car list-of-coordinates))
	      (rest-of-list
	       (cdr list-of-coordinates)))
	  (iter
	   (cdr list-of-coordinates)
	   (append
	    (map
	     (λ (queen-b-coordinates)
	       (if
		(or
		 (same-row-col?
		  queen-a-coordinates
		  queen-b-coordinates)
		 (same-diagonal?
		  queen-a-coordinates
		  queen-b-coordinates))
		1
		0))
	     rest-of-list)
	    result)))))    
  (let ((list-of-coordinates
	 (map
	  cons
	  chromosome
	  (enumerate-interval minimum maximum))))
    (- max-atacking-pairs
       (sum
	(iter
	 list-of-coordinates
	 '())))))


(define (select-parent pop-probs-pair draw)
  (if (null? pop-probs-pair)
      '()
      ((λ (parent probability)
	 (if (<= draw probability)
	     parent
	     (select-parent
	      (cdr pop-probs-pair)
	      draw)))
       (car (car pop-probs-pair))
       (cdr (car pop-probs-pair)))))


(define (roulette-select-parents population probabilities qtd-parents)
  (define (iter pop-probs-pair qtd-parents parents)
    (if (= qtd-parents 0)
	parents
	(let ((parent (select-parent pop-probs-pair (random:uniform))))
	  (iter
	   pop-probs-pair
	   (- qtd-parents 1)
	   (append  parents (list parent))))))
  (let ((pop-probs-pair
	 (map
	  cons
	  population
	  probabilities)))
    (iter
     pop-probs-pair
     qtd-parents
     (list ))))

(define (reproduce x y)
  ((λ (point)
    (append
     (take x point)
     (drop y point)))
   (+ 1
      (random
       (- (length x) 1)))))

(define (update-list lst index value)
  (define (iter i old-list new-list)
    (cond ((null? old-list) new-list)
	  ((= i index) (iter (+ i 1)
			     (cdr old-list)
			     (append new-list (list value))))
	  (else
	   (iter (+ i 1)
		 (cdr old-list)
		 (append new-list (list (car old-list)))))))
  (iter 0 lst (list )))

(define (mutate chromosome)
  (define (iter new-value bound point)
    (if (= new-value
	   (list-ref chromosome point))
	(iter (+ 1 (random bound))
	      bound
	      point)
	(update-list chromosome point new-value)))
  ((λ (bound)
    (iter (+ 1 (random bound))
	  bound
	  (random bound)))
  (length chromosome)))


(define (ga-eight-queen population fitness-fn epochs)
  (define (iter i new-population)
    (let ((individual-fitness (map fitness-fn new-population))
	  (total-fitness (sum (map fitness-fn new-population))))
      (if (or (= i epochs)
	      (member max-atacking-pairs individual-fitness))
	  (list new-population individual-fitness)
	  (let ((probabilities (make-roulette-slices-list
				(map (λ (fitness) (/ fitness total-fitness))
				     individual-fitness))))
	    (iter
	     (+ i 1)
	     (map (λ (x)
		    (let ((parents
			   (roulette-select-parents
			    new-population
			    probabilities
			    qtd-parents)))
		      (if (<= (random:uniform)
			      mutation_probability)
			  (mutate
			   (reproduce
			    (car parents)
			    (cadr parents)))
			  (reproduce
			   (car parents)
			   (cadr parents)))))
		  new-population))))))
  (iter 0 population))

(define (make-roulette-slices-list percentage-fitness-list)
  (define (iter percentage-fitness slices-list  untill) 
    (if (null? percentage-fitness)
	slices-list
	(iter
	 (cdr percentage-fitness)
	 (append slices-list
		 (list
		  (sum (take percentage-fitness-list untill))))
	 (+ untill 1))))
  (iter
   percentage-fitness-list
   (list )
   1))

;; O número máximo de rainhas em xeque em um
;; tabuleiro NxN é dado pelo coeficiente
;; Binominal (n, 2). 
(define board-size 8)
(define qtd-parents 2)
(define mutation_probability 0.1)
(define population-size 1000)
(define chromosome-size board-size)
(define minimum 1)
(define maximum board-size)
(define epochs 100)
(define max-atacking-pairs (comb board-size 2))
(define population
  (create-population population-size minimum maximum chromosome-size))

(define final-population
  (ga-eight-queen population fitness-fn epochs))

(filter (λ (c) (= (fitness-fn c) max-atacking-pairs)) (car final-population))

(map fitness-fn (car final-population))
