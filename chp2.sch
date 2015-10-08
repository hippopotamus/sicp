;2.17
(define (last-pair lst)
	(if (null? (cdr lst))
			(car lst)
			(last-pair (cdr lst))))
;2.18
(define (reverse items)
 (if (null? items)
     items
     (append (reverse (cdr items)) (list (car items)))))
;2.19
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (first-denomination coin-values) (car coin-values))
(define (except-first-denomination coin-values) (cdr coin-values))
(define (no-more? coin-values) (null? coin-values))

(define(cc amount coin-values)
	(cond ((= amount 0) 1)
				((or (< amount 0) (no-more? coin-values)) 0)
				(else
					(+ (cc amount (except-first-denomination coin-values))
						 (cc (- amount (first-denomination coin-values)) coin-values)))))

(cc 100 us-coins)
;2.20
(define (my-filter filter-by lst)
	(if (null? lst)
			lst
			(if (filter-by (car lst))
					(cons (car lst) (my-filter filter-by (cdr lst)))
					(my-filter filter-by (cdr lst)))))

(define (same-parity x . y)
	(cons x (if (even? x)
							(my-filter even? y)
							(my-filter odd? y))))
;2.21
(define (square-list items)
(if (null? items)
		items
		(cons (* (car items) (car items)) (square-list (cdr items)))))

(define (square-list items)
	(map (lambda (x) (* x x)) items))
;2.22 because he's an asshole
(define (square-list items)
	(define (iter things answer)
		(if (null? things)
				answer
				(iter (cdr things) (cons (square (car things)) answer))))
(iter items ())) ;backwards b/c of the order of arguments
(define (square-list items)
	(define (iter things answer)
		(if (null? things)
				answer
				(iter (cdr things) (cons answer (square (car things))))))
	(iter items ())) ;nested b/c list is the first arg. if you want a flat list, the first element needs to be an atom, not a list
;2.23
(define (for-each fn lst)
	(cond ((null? lst) #t)
				(else (fn (car lst))
							(for-each fn (cdr lst)))))
(for-each (lambda (x) (newline) (display x)) (list 57 321 88))
(list 1 (list 2 (list 3 4)))
;2.24
; 1 2
;	   3 4
;2.25
(car (cdr (car (cdr (cdr (list 1 3 (list 5 7) 9))))))
(car (car (list (list 7))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7))))))))))))))))))
;2.26
(define x (list 1 2 3))
(define y (list 4 5 6))
(append x y) ; => (1 2 3 4 5 6)
(cons x y) 	 ; => ((1 2 3) 4 5 6)
(list x y) 	 ; => ((1 2 3) (4 5 6))
'(x y) 			 ; => (x y)
;2.27
(define (reverse items)
 (if (null? items)
     items
     (append (reverse (cdr items))
     				 (if (pair? (car items))
	 							 (list (reverse (car items)))
	 							 (list (car items))))))
(reverse (list (list 1 2) (list 3 4)))
;2.28
(define (atom? x) (not (pair? x)))
(define (fringe tree)
	(cond ((null? tree) '())
				((atom? tree) (list tree))
				(else (append (fringe (car tree)) (fringe (cdr tree))))))
(fringe (list (list 1 2) (list 3 4)))
;2.29
(define (make-mobile left right) (cons left right))
(define (make-branch length structure) (cons length structure))
(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cdr mobile))
(define (branch-length branch) (car branch))
(define (branch-structure branch) (cdr branch))
(define (total-weight mobile)
	(+ (branch-weight (left-branch mobile)) (branch-weight (right-branch mobile))))
(define (branch-weight branch)
	(if (pair? (branch-structure branch))
						 (total-weight (branch-structure branch))
						 (branch-structure branch)))
(define (branch-torque branch)
			(* (branch-length branch) (branch-weight branch)))
(define (branch-balanced? branch)
	(if (pair? (branch-structure branch))
			(balanced? (branch-structure branch))
			#t))
(define (balanced? mobile)
	(and
		(= (branch-torque (left-branch mobile)) (branch-torque (right-branch mobile)))
		(branch-balanced? (left-branch mobile))
		(branch-balanced? (right-branch mobile))))
(define a-mobile
	(make-mobile
		(make-branch 2
			(make-mobile
				(make-branch 1 3)
				(make-branch 1 3)))
		(make-branch 3
			(make-mobile
				(make-branch 2 2)
				(make-branch 2
					(make-mobile
						(make-branch 1 1)
						(make-branch 1 1)))))))
; right branch stuff doesn't need car in front of cdr with cons instead of list
;2.30
(define (scale-tree tree factor)
	(cond
		((null? tree) nil)
		((not (pair? tree))
		(* tree factor))
		(else
			(cons (scale-tree (car tree) factor)
						(scale-tree (cdr tree) factor)))))
(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)
(define (square-tree tree)
	(cond
		((null? tree) '())
		((not (pair? tree))
			(* tree tree))
		(else
			(cons (square-tree (car tree))
						(square-tree (cdr tree))))))
(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
(define (square-tree tree)
	(map (lambda (sub-tree)
		(if (pair? sub-tree)
				(square-tree sub-tree)
				(* sub-tree sub-tree)))
		tree))
(define (tree-map fn tree)
	(map (lambda (sub-tree)
		(if (pair? sub-tree)
				(tree-map fn sub-tree)
				(fn sub-tree)))
		tree))
(define square (lambda (x) (* x x)))
(define (square-tree tree) (tree-map square tree))
;2.32
(define (subsets s)
	(if (null? s)
			(list '())
			(let ((rest (subsets (cdr s))))
				(append rest (map (lambda (x)
					(cons (car s) x))
					rest)))))
(subsets (list 1 2 3))
;2.33
(define (accumulate op initial sequence)
	(if (null? sequence)
			initial
			(op (car sequence) (accumulate op initial (cdr sequence)))))
(define (map p sequence)
	(accumulate (lambda (x y) (cons (p x) y)) '() sequence))
(define (append2 seq1 seq2)
	(accumulate cons seq2 seq1))
(define (length sequence)
	(accumulate (lambda (x y) (+ 1 y)) 0 sequence))
;2.34
(define (horner-eval x coefficient-sequence)
	(accumulate
		(lambda (this-coeff higher-terms)
		(+ this-coeff (* x higher-terms)))
		0
		coefficient-sequence))
(horner-eval 2 (list 1 3 0 5 0 1))
;2.35
(define (count-leaves x)
	(cond ((null? x) 0)
				((not (pair? x)) 1)
				(else (+ (count-leaves (car x)) (count-leaves (cdr x))))))
(define (flatten-tree t)
	(cond ((null? t) '())
				((not (pair? t)) (list t))
				(else (append (flatten-tree (car t)) (flatten-tree (cdr t))))))
(define (count-leaves t)
	(accumulate + 0 (map (lambda (x) 1) (flatten-tree t))))
;2.36
(define (accumulate-n op init seqs)
	(if (null? (car seqs))
			'()
			(cons (accumulate op init (map car seqs))
						(accumulate-n op init (map cdr seqs)))))
(accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
;2.37
(define (accumulate op initial sequence)
	(if (null? sequence)
			initial
			(op (car sequence) (accumulate op initial (cdr sequence)))))
(define m (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))
(define (dot-product v w)
	(accumulate + 0 (map * v w)))
(dot-product (list 1 2 3) (list 4 5 6))
(define (matrix-*-vector m v)
	(map (lambda (row) (dot-product v row)) m))
(define (matrix-*-matrix m n)
	(map (lambda (v w) (dot-product v w)) m n))
(define (transpose m)
	(accumulate-n cons '() m))
(define (matrix-*-matrix m n)
	(let ((cols (transpose n)))
		(map (lambda (row) (matrix-*-vector cols row)) m)))
;2.38
(define (fold-right op initial sequence)
	(if (null? sequence)
			initial
			(op (car sequence) (accumulate op initial (cdr sequence)))))

(define (fold-left op initial sequence)
	(define (iter result rest)
		(if (null? rest)
				result
				(iter (op result (car rest)) (cdr rest))))
	(iter initial sequence))
(fold-right / 1 (list 1 2 3)) ; 3/2
(fold-left / 1 (list 1 2 3)) ; 1/6
(fold-right list '() (list 1 2 3)) ; (1 (2 (3 ())))
(fold-left list '() (list 1 2 3)) ; (((() 1) 2) 3)
;2.39
(define (reverse sequence)
	(fold-right (lambda (x y) (append y (list x))) '() sequence))
(define (reverse sequence)
	(fold-left (lambda (x y) (cons y x)) '() sequence))
;2.40
(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))
(accumulate append '()
	(map (lambda (i)
		(map (lambda (j) (list i j))
			(enumerate-interval 1 (- i 1))))
		(enumerate-interval 1 n)))
(define (flatmap proc seq)
	(accumulate append '() (map proc seq)))
(define (next x)
  (if (= x 2) 3 (+ x 2)))
(define (square x) (* x x))
(define (divides? a b) (= (remainder b a) 0))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
(define (smallest-divisor n) (find-divisor n 2))
(define (prime? x) (= (smallest-divisor x) x))
(define (prime-sum? pair)
	(prime? (+ (car pair) (cadr pair))))
(define (make-pair-sum pair)
	(list (car pair) (cadr pair) (+ (car pair) (cadr pair))))
(define (unique-pairs n)
	(flatmap (lambda (i) (map (lambda (j) (list i j)) (enumerate-interval 1 (- i 1))))
		(enumerate-interval 1 n)))
(define (prime-sum-pairs n)
	(map make-pair-sum
		(filter prime-sum? (unique-pairs n))))
(define (permutations s)
	(if (null? s)
			(list '())
			(flatmap (lambda (x)
				(map (lambda (p) (cons x p))
					(permutations (remove x s))))
				s)))
(define (remove item sequence)
	(filter (lambda (x) (not (= x item))) sequence))
;2.41
(define (make-triple-sum triple)
	(append triple (list (+ (car triple) (cadr triple) (cadr (cdr triple))))))
(define (unique-triples n)
	(flatmap
		(lambda (i)
			(flatmap (lambda (j) (map (lambda (k) (list i j k)) (enumerate-interval 1 (- j 1))))
			(enumerate-interval 1 (- i 1))))
		(enumerate-interval 1 n)))
(define (sum-triples-equals? n value)
	(define (triple-sum? triple)
		(= (+ (car triple) (cadr triple) (cadr (cdr triple))) value))
	(map make-triple-sum (filter triple-sum? (unique-triples n))))
;2.42
(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))
(define (flatmap proc seq)
	(accumulate append '() (map proc seq)))
(define (queens board-size)
	(define (queen-cols k)
		(if (= k 0)
				(list empty-board)
				(filter
					(lambda (positions) (safe? k positions))
					(flatmap
						(lambda (rest-of-queens)
							(map
								(lambda (new-row) (adjoin-position new-row k rest-of-queens))
								(enumerate-interval 1 board-size)))
						(queen-cols (- k 1))))))
	(queen-cols board-size))
(define (adjoin-position row k other-positions)
	(append (list (cons row k)) other-positions))
(define empty-board '())
(define (safe? col positions)
	(cond ))
;2.44
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))
(define (right-split painter n)
	(if (= n 0)
			painter
			(let ((smaller (right-split painter (- n 1))))
				(beside painter (below smaller smaller)))))
(define (corner-split painter n)
	(if (= n 0)
			painter
			(let ((up (up-split painter (- n 1)))
						(right (right-split painter (- n 1))))
				(let ((top-left (beside up up))
							(bottom-right (below right right))
							(corner (corner-split painter (- n 1))))
					(beside (below painter top-left)
									(below bottom-right corner))))))
(define (up-split painter n)
	(if (= n 0)
			painter
			(let ((smaller (up-split painter (- n 1))))
				(below painter (beside smaller smaller)))))
;2.45
(define (split dir split-dir)
	(lambda (painter n)
		(if (= n 0)
				painter
				(let ((smaller (split painter (- n 1))))
					(split-dir painter (dir smaller smaller))))))
(define right-split (split beside below))
(define up-split (split below beside))
;2.46
(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))
(define (add-vect v1 v2)
	(make-vect (+ (xcor-vect v1) (xcor-vect v2)) (+ (ycor-vect v1) (ycor-vect v2))))
(define (sub-vect v1 v2)
	(make-vect (- (xcor-vect v1) (xcor-vect v2)) (- (ycor-vect v1) (ycor-vect v2))))
(define (scale-vect s v)
	(make-vect (* s (xcor-vect v)) (* s (ycor-vect v))))
;2.47
(define (make-frame origin edge1 edge2)
	(list origin edge1 edge2))
(define (frame-origin frame) (car frame))
(define (frame-edge1 frame) (cadr frame))
(define (frame-edge2 frame) (cadr (cdr frame)))
;----
(define (make-frame origin edge1 edge2)
	(cons origin (cons edge1 edge2)))
(define (frame-edge2 frame) (cdr (cdr frame)))
;2.48
(define (segments->painter segment-list)
	(lambda (frame)
		(for-each
			(lambda (segment)
			(draw-line
				((frame-coord-map frame) (start-segment segment))
				((frame-coord-map frame) (end-segment segment))))
			segment-list)))
(define (make-segment v1 v2) (cons v1 v2))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))
;2.49
;a
(define outline-segments
	(let ((bl (make-vect 0.0 0.0))
				(br (make-vect 0.99 0.0))
				(tl (make-vect 0.0 0.99))
				(tr (make-vect 0.99 0.0)))
		(list
			(make-segment bl br)
			(make-segment br tr)
			(make-segment tr tl)
			(make-segment tl bl))))
(segments->painter outline-segments)
;b
(define x-shape
	(let ((bl (make-vect 0.0 0.0))
				(br (make-vect 0.99 0.0))
				(tl (make-vect 0.0 0.99))
				(tr (make-vect 0.99 0.99)))
		(list
			(make-segment bl tr)
			(make-segment tl br))))
;c
(define diamond
	(let ((top (make-vect 0.5 0.99))
				(right (make-vect 0.99 0.5))
				(bottom (make-vect 0.5 0.0))
				(left (make-vect 0.0 0.5)))
		(list
			(make-segment top right)
			(make-segment right bottom)
			(make-segment bottom left)
			(make-segment left top))))
;d no thanks
;2.50
;have to use different params, b/c of the way it's defined with the library
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

(define (flip-horizontal painter)
	(transform-painter
		(make-vect 1.0 0.0)
		(make-vect 0.0 0.0)
		(make-vect 1.0 1.0)) painter)
(define (rotate-180 painter)
	(transform-painter
		(make-vect 1.0 1.0)
		(make-vect 0.0 1.0)
		(make-vect 1.0 0.0)) painter)
(define (rotate-270 painter)
	(transform-painter
		(make-vect 0.0 1.0)
		(make-vect 0.0 0.0)
		(make-vect 1.0 1.0)) painter)
;2.51
(define (below-a painter1 painter2)
	(let ((split-point (make-vect 0.0 0.5)))
		(let
			((paint-top
				((transform-painter split-point  (make-vect 1.0 0.5) (make-vect 0.0 1.0)) painter1))
			(paint-bottom
				((transform-painter (make-vect 0.0 0.0) (make-vect 1.0 0.0)  split-point) painter2)))
			(lambda (frame)
				(paint-top frame)
				(paint-bottom frame)))))
(define (rotate-90 painter)
		(transform-painter
		(make-vect 1.0 0.0)
		(make-vect 1.0 1.0)
		(make-vect 0.0 0.0)) painter)

(define (below-b painter1 painter2)
	(rotate-90 (beside (rotate-270 painter1) (rotate-270 painter2))))
;2.53
(define (memq item x)
	(cond ((null? x) false)
				((eq? item (car x)) x)
				(else (memq item (cdr x)))))
(a b c)
((george))
((y1 y2))
(y1 y2)
#f
#f
(red shoes blue socks)
;2.54
(define (equals? a b)
	(cond ((or (null? a) (null? b)) (eq? a b))
				((or (and (pair? (car a)) (not (pair? (car b))))
						 (and (pair? (car b)) (not (pair? (car a))))) #f)
				((pair? (car a)) (and (equals? (car a) (car b)) (equals? (cdr a) (cdr b))))
				((eq? (car a) (car b)) (equals? (cdr a) (cdr b)))))
;2.55
; the interpreter is interpreting everything after the first quote as a list,
; the symbol quote (which ' is syntactic sugar for) is the first arg
;2.56
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
	(and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? exp num)
	(and (number? exp) (= exp num)))

(define (sum? x)
	(and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s)
	(define (sumation x sum)
		(if (null? x) sum (sumation (cdr x) (make-sum (car x) sum))))
	(sumation (cddr s) 0))
(define (make-sum a1 a2)
	(cond ((=number? a1 0) a2)
				((=number? a2 0) a1)
				((and (number? a1) (number? a2)) (+ a1 a2))
				(else (list '+ a1 a2))))

(define (product? x)
	(and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p)
	(define (multiply x product)
		(if (null? x) product (multiply (cdr x) (make-product (car x) product))))
	(multiply (cddr p) 1))
(define (make-product m1 m2)
	(cond ((or (=number? m1 0) (=number? m2 0)) 0)
				((=number? m1 1) m2)
				((=number? m2 1) m1)
				((and (number? m1) (number? m2)) (* m1 m2))
				(else (list '* m1 m2))))

(define (exponentiation? x)
	(and (pair? x) (eq? (car x) '**)))
(define (base exponentiation) (cadr exponentiation))
(define (exponent exponentiation) (caddr exponentiation))
(define (make-exponentiation b x)
	(cond ((=number? x 0) 1)
				((=number? x 1) b)
				((and (number? b) (number? x)) (expt b x))
				(else (list '** b x))))

(define (deriv exp var)
	(cond ((number? exp) 0)
				((variable? exp) (if (same-variable? exp var) 1 0))
				((sum? exp)
					(make-sum
						(deriv (addend exp) var)
						(deriv (augend exp) var)))
				((product? exp)
					(make-sum
						(make-product
							(multiplier exp)
							(deriv (multiplicand exp) var))
						(make-product
							(deriv (multiplier exp) var)
							(multiplicand exp))))
				((exponentiation? exp)
					(make-product
						(make-product
							(exponent exp)
							(make-exponentiation (base exp) (make-sum (exponent exp) -1)))
						(deriv (base exp) var)))
				(else (error "unknown expression type -- DERIV" exp))))
(deriv '(* x y (+ x 3)) 'x)
;2.58
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
	(and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? exp num)
	(and (number? exp) (= exp num)))

(define (exponentiation? x)
	(and (pair? x) (eq? (car x) '**)))
(define (base exponentiation) (cadr exponentiation))
(define (exponent exponentiation) (caddr exponentiation))
(define (make-exponentiation b x)
	(cond ((=number? x 0) 1)
				((=number? x 1) b)
				((and (number? b) (number? x)) (expt b x))
				(else (list '** b x))))
;a
(define (addend s) (car s))
(define (sum? x)
	(and (pair? x) (eq? (cadr x) '+)))
(define (augend s) (caddr s))
(define (make-sum a1 a2)
	(cond ((=number? a1 0) a2)
				((=number? a2 0) a1)
				((and (number? a1) (number? a2)) (+ a1 a2))
				(else (list a1 '+ a2))))

(define (multiplier p) (car p))
(define (product? x)
	(and (pair? x) (eq? (cadr x) '*)))
(define (multiplicand p) (caddr p))
(define (make-product m1 m2)
	(cond ((or (=number? m1 0) (=number? m2 0)) 0)
				((=number? m1 1) m2)
				((=number? m2 1) m1)
				((and (number? m1) (number? m2)) (* m1 m2))
				(else (list m1 '* m2))))
;b
(define (addend s) (car s))
(define (sum? x)
	(and (pair? x) (eq? (cadr x) '+)))
(define (augend s)
	(define (sumation x sum)
		(if (null? (cdr x)) (make-sum (car x) sum) (sumation (cddr x) (make-sum (car x) sum))))
	(sumation (cddr s) 0))
(define (make-sum a1 a2)
	(cond ((=number? a1 0) a2)
				((=number? a2 0) a1)
				((and (number? a1) (number? a2)) (+ a1 a2))
				(else (list a1 '+ a2))))

(define (multiplier p) (car p))
(define (product? x)
	(and (pair? x) (eq? (cadr x) '*)))
(define (multiplicand p)
	(define (multiply x product)
		(if (null? (cdr x)) (make-product (car x) product) (multiply (cddr x) (make-product (car x) product))))
	(multiply (cddr p) 1))
(define (make-product m1 m2)
	(cond ((or (=number? m1 0) (=number? m2 0)) 0)
				((=number? m1 1) m2)
				((=number? m2 1) m1)
				((and (number? m1) (number? m2)) (* m1 m2))
				(else (list m1 '* m2))))
;2.59
(define (element-of-set? x set)
	(cond ((null? set) false)
				((equal? x (car set)) true)
				(else (element-of-set? x (cdr set)))))
(define (adjoin-set x set)
	(if (element-of-set? x set)
			set
			(cons x set)))
(define (intersection-set set1 set2)
	(cond ((or (null? set1) (null? set2)) '())
				((element-of-set? (car set1) set2)
					(cons (car set1)
								(intersection-set (cdr set1) set2)))
				(else (intersection-set (cdr set1) set2))))
(define (union-set set1 set2)
	(if (null? set1)
			set2
			(union-set (cdr set1) (adjoin-set (car set1) set2))))
;2.60
(define (adjoin-set x set) (cons x set))
;2.61
(define (element-of-set? x set)
	(cond ((null? set) false)
				((= x (car set)) true)
				((< x (car set)) false)
				(else (element-of-set? x (cdr set)))))
(define (adjoin-set x set)
	(cond ((null? set) (list x))
				((= x (car set)) set)
				((< x (car set)) (cons x set))
				(else (cons (car set) (adjoin-set x (cdr set))))))
(define (intersection-set set1 set2)
	(if (or (null? set1) (null? set2))
			'()
			(let ((x1 (car set1)) (x2 (car set2)))
				(cond ((= x1 x2)
								(cons x1 (intersection-set (cdr set1) (cdr set2))))
							((< x1 x2) (intersection-set (cdr set1) set2))
							((< x2 x1) (intersection-set set1 (cdr set2)))))))
;2.62
(define (union-set set1 set2)
	(if (null? set1)
			set2
			(union-set (cdr set1) (adjoin-set (car set1) set2))))
;2.63
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
	(list entry left right))
(define (tree->list-1 tree)
	(if (null? tree)
			'()
			(append
				(tree->list-1 (left-branch tree))
				(cons (entry tree) (tree->list-1 (right-branch tree))))))
(define (tree->list-2 tree)
	(define (copy-to-list tree result-list)
		(if (null? tree)
				result-list
				(copy-to-list
					(left-branch tree)
					(cons
						(entry tree)
						(copy-to-list (right-branch tree) result-list)))))
	(copy-to-list tree '()))
; same results for both
; if append is recursive (which i believe it is in this implementation) O(logn) time and O(n*logn) space
; for tree->list2 O(n) time O(n) space
;2.64
(define (list->tree elements)
	(car (partial-tree elements (length elements))))
(define (partial-tree elts n)
	(if (= n 0)
			(cons '() elts)
			(let ((left-size (quotient (- n 1) 2)))
				(let ((left-result (partial-tree elts left-size)))
					(let ((left-tree (car left-result))
								(non-left-elts (cdr left-result))
								(right-size (- n (+ left-size 1))))
						(let ((this-entry (car non-left-elts))
									(right-result (partial-tree (cdr non-left-elts)
									right-size)))
							(let ((right-tree (car right-result))
										(remaining-elts (cdr right-result)))
								(cons (make-tree this-entry left-tree right-tree) remaining-elts))))))))
;a
it continually cuts the list -1 in half. the middle 1 goes as the element, other half to the right
it recurses until only the first element of the list is left, that becomes the outermost left branch
once it comes out, the left overs in whatever list is there go to the right. if there is more than 1 element,
it the same recursion above happens. this continues where it comes out of the stack and goes deeper
into the stack from left to right.
(1 3 5 7 9 11) ;left-size for the first iteration is (quotient (-6 1) 2) = 2 => first entry is 5
			5
		 / \
		1		9
		 \ / \
		 3 7 11

;b
O(n) ; it visits each element once and calls cons
;2.65
;(define (element-of-set? x set)
;	(cond ((null? set) false)
;				((= x (entry set)) #t)
;				((< x (entry set)) (element-of-set? x (left-branch set)))
;				((> x (entry set)) (element-of-set? x (right-branch set)))))
;(define (adjoin-set x set)
;	(cond ((null? set) (make-tree x '() '()))
;				((= x (entry set)) set)
;				((< x (entry set)) (make-tree (entry set) (adjoin-set x (left-branch set)) (right-branch set)))
;				((> x (entry set)) (make-tree (entry set) (left-branch set) (adjoin-set x (right-branch set))))))
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
	(list entry left right))
(define (list->tree elements)
	(car (partial-tree elements (length elements))))
(define (partial-tree elts n)
	(if (= n 0)
			(cons '() elts)
			(let ((left-size (quotient (- n 1) 2)))
				(let ((left-result (partial-tree elts left-size)))
					(let ((left-tree (car left-result))
								(non-left-elts (cdr left-result))
								(right-size (- n (+ left-size 1))))
						(let ((this-entry (car non-left-elts))
									(right-result (partial-tree (cdr non-left-elts)
									right-size)))
							(let ((right-tree (car right-result))
										(remaining-elts (cdr right-result)))
								(cons (make-tree this-entry left-tree right-tree) remaining-elts))))))))
(define (tree->list tree)
	(define (copy-to-list tree result-list)
		(if (null? tree)
				result-list
				(copy-to-list
					(left-branch tree)
					(cons
						(entry tree)
						(copy-to-list (right-branch tree) result-list)))))
	(copy-to-list tree '()))
(define (element-of-set? x set)
	(cond ((null? set) false)
				((= x (car set)) true)
				((< x (car set)) false)
				(else (element-of-set? x (cdr set)))))
(define (adjoin-set x set)
	(cond ((null? set) (list x))
				((= x (car set)) set)
				((< x (car set)) (cons x set))
				(else (cons (car set) (adjoin-set x (cdr set))))))

(define (intersection-set set1 set2)
	(define (intersection-list list1 list2)
		(if (or (null? list1) (null? list2))
				'()
				(let ((x1 (car list1)) (x2 (car list2)))
					(cond ((= x1 x2)
									(cons x1 (intersection-list (cdr list1) (cdr list2))))
								((< x1 x2) (intersection-list (cdr list1) list2))
								((< x2 x1) (intersection-list list1 (cdr list2)))))))
	(list->tree (intersection-list (tree->list set1) (tree->list set2))))

(define (union-set set1 set2)
	(define (union-list list1 list2)
		(if (null? list1)
				list2
				(union-list (cdr list1) (adjoin-set (car set1) list2))))
	(list->tree (union-list (tree->list set1) (tree->list set2))))
;2.66
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (lookup given-key set-of-records)
	(cond ((null? set-of-records) #f)
				((equal? given-key (key (entry set-of-records))) (entry set-of-records))
				((< given-key (key (entry set-of-records))) (lookup given-key (left-branch set-of-records)))
				((> given-key (key (entry set-of-records))) (lookup given-key (right-branch set-of-records)))))
;2.67
(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))
(define (make-code-tree left right)
	(list left right (append (symbols left) (symbols right)) (+ (weight left) (weight right))))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
	(if (leaf? tree)
			(list (symbol-leaf tree))
			(caddr tree)))
(define (weight tree)
	(if (leaf? tree)
			(weight-leaf tree)
			(cadddr tree)))
(define (decode bits tree)
	(define (decode-1 bits current-branch)
		(if (null? bits)
				'()
				(let ((next-branch (choose-branch (car bits) current-branch)))
					(if (leaf? next-branch)
							(cons
								(symbol-leaf next-branch)
								(decode-1 (cdr bits) tree))
							(decode-1 (cdr bits) next-branch)))))
	(decode-1 bits tree))
(define (choose-branch bit branch)
	(cond ((= bit 0) (left-branch branch))
				((= bit 1) (right-branch branch))
				(else (error "bad bit -- CHOOSE-BRANCH" bit))))
(define sample-tree
	(make-code-tree
		(make-leaf 'A 4)
		(make-code-tree
			(make-leaf 'B 2)
			(make-code-tree
				(make-leaf 'D 1)
				(make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(decode sample-message sample-tree)
;2.68
(define (memq item x)
	(cond ((null? x) false)
				((eq? item (car x)) x)
				(else (memq item (cdr x)))))

(define (encode message tree)
	(if (null? message)
			'()
			(append
				(encode-symbol (car message) tree)
				(encode (cdr message) tree))))
(define (encode-symbol symbol tree)
		(cond ((leaf? tree) '())
					((memq symbol (symbols (left-branch tree)))
						(cons 0 (encode-symbol symbol (left-branch tree))))
					((memq symbol (symbols (right-branch tree)))
						(cons 1 (encode-symbol symbol (right-branch tree))))
					(else (error "symbol not in tree:" symbol))))
;2.69
(define (adjoin-set x set)
	(cond ((null? set) (list x))
				((< (weight x) (weight (car set))) (cons x set))
				(else (cons (car set)
					(adjoin-set x (cdr set))))))
(define (make-leaf-set pairs)
	(if (null? pairs)
			'()
			(let ((pair (car pairs)))
				(adjoin-set (make-leaf (car pair) ; symbol
						(cadr pair)) ; frequency
					(make-leaf-set (cdr pairs))))))
(define (make-code-tree left right)
	(list left right (append (symbols left) (symbols right)) (+ (weight left) (weight right))))
(define (generate-huffman-tree pairs)
	(successive-merge (make-leaf-set pairs)))
(define (successive-merge ordered-leaf-set)
	(if (= (length ordered-leaf-set) 1)
			(car ordered-leaf-set)
			(successive-merge (adjoin-set
				(make-code-tree (car ordered-leaf-set) (cadr ordered-leaf-set))
				(cddr ordered-leaf-set)))))
(define test-tree (generate-huffman-tree '((A 3) (B 5) (C 6) (D 6))))
(encode '(A B C D) test-tree)
;2.70
(define 1950s (generate-huffman-tree '((BOOM 1) (WAH 1) (A 2) (GET 2) (JOB 2) (SHA 3) (YIP 9) (NA 16))))
(define lyrics '(GET A JOB SHA na na na na na na na na get a job Sha na na na na na na na na
Wah yip yip yip yip yip yip yip yip yip
Sha boom))
(encode lyrics 1950s)
84 bits, fixed length 108 bits
;2.71
1, (- n 1) --  each new element is 1 larger than the sum of all of the rest of them
;2.72
n^2
;2.73
;a
the logic is different for those two
;b
;TODO i spent too much time away from this. hopefully i remember to come back to it.

;2.74
;a
(define (get-record division employee-name)
	((get division 'record) employee-name))
;b
(define (get-salary division record)
	((get division 'salary) record))
;c
(define (find-employee-record employee-name files)
	(if (null? files)
			#f
			(or (get-record (car files) employee-name)
					(find-employee-record find-employee-record (cdr files)))))
;d
the new company needs to add the methods to find whatever Insatiable searches for
plus type tags to match their methods to their db

;2.75
(define (make-from-mag-ang r a)
	(define (dispatch op)
		(cond ((eq? op 'real-part) (* r (cos a)))
					((eq? op 'image-part) (* r (sin a)))
					((eq? op 'magnitude) r)
					((eq? op 'angle) a)
					(else (error "Unknown op -- MAKE_MAG_FROM_ANG" op))))
	dispatch)
