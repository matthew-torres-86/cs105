;;
;; CS105 Homework 4
;; HOFS
;; Matthew Torres
;;

;;
;; Problem 2
;;

;; (flip f) takes a function and returns a function in which the arguments are 
;; flipped

;; laws:
;; ((flip f) x y) == (f y x)

(define flip (f)
    (lambda (x y) (f y x)))

        (check-assert (not ((flip <) 3 4)))
        (check-assert (not ((flip <=) 3 4)))
        (check-expect ((flip append) '(a b c) '(1 2 3)) '(1 2 3 a b c))


;;
;; Problem 3
;;

;; (takewhile p? xs) takes a predicate and a list and returns the longest 
;; prefix of the list in which every element satisfies the predicate

;; laws:
;; (takewhile p? xs) == '(), where p? is false
;; (takewhile p? xs) == (cons (car xs) (takewhile p? (cdr xs))),
;;                  where p? is true

(define takewhile (p? xs)
    (if (p? (car xs))
        (cons (car xs) (takewhile p? (cdr xs)))
        '()))

        (define even? (x) (= (mod x 2) 0))
        (check-expect (takewhile even? '(2 4 6 7 8 10 12)) '(2 4 6))

;; (dropwhile p? xs) takes a predicate and a list and removes the longest 
;; prefix of the list in which every element satisfies the predicate, 
;; and returns the remainder of the list

;; laws:
;; (dropwhile p? xs) == xs, where p? is false
;; (takewhile p? xs) == (dropwhile p? (cdr xs)), where p? is true

(define dropwhile (p? xs)
    (if (p? (car xs))
        (dropwhile p? (cdr xs))
        xs))

        (check-expect (dropwhile even? '(2 4 6 7 8 10 12)) '(7 8 10 12))


;;
;; Problem 4
;;

;; (ordered-by? precedes?) takes a comparison function, and returns a predicate 
;; that tells if a list of values is totally ordered by that relation

;; laws:
;; (ordered-by? precedes?) '()) == #t
;; (ordered-by? precedes?) '(x)) == #t, where x is a single element
;; ((ordered-by? precedes?) (cons a (cons b bs))) == 

(define ordered-by? (precedes?)
    (lambda (xs)
        (if (null? xs)
            #t
            (if (null? (car xs))
                #t
                (if (null? (cdr xs))
                    #t
                    (if (precedes? (car xs) (car (cdr xs)))
                        ((ordered-by? precedes?) (cdr xs))
                        #f))))))

        (check-assert ((ordered-by? <) '()))
        (check-assert ((ordered-by? <) '(1)))
        (check-assert (not ((ordered-by? >) '(2 3))))
        (check-assert ((ordered-by? <) '(1 2 3)))
        (check-assert (not ((ordered-by? >) '(1 2 3))))
        (check-assert ((ordered-by? =) '(3 3 3)))


;;
;; Problem 5
;;

;; (max* xs) returns the maximum of a non-empty list of integers.
(define max* (xs)
    (foldr max -100000 xs))

        (check-expect (max* '(1000 200 -35 42)) 1000)

;; (sum xs) returns the sum of a non-empty list of integers.
(define sum (xs)
    (foldr + 0 xs))

        (check-expect (sum '(1000 200 -35 42)) 1207)

;; (product xs) returns the product of a non-empty list of integers.
(define product (xs)
    (foldr * 1 xs))

        (check-expect (product '(1000 200 -35 42)) -294000000)


;;
;; Problem 6
;;

;; (reverse xs) appends two lists.
(define append (xs ys)
    (foldr cons ys xs))

        (check-expect (append '(1 2 3 4) '(5 6 7 8)) '(1 2 3 4 5 6 7 8))
        (check-expect (append '() '()) '())

;; (reverse xs) returns the reverse of a list.
(define reverse (xs)
    (foldl cons '() xs))

        (check-expect (reverse '(1 2 3 4)) '(4 3 2 1))
        (check-expect (reverse '()) '())

;;
;; Problem 7
;;

;; (map f xs) applies a function to each element in a list and then returns the 
;; resulting list.

(define map (f xs)
    (foldr (lambda (x y) (cons (f x) y)) '() xs))

        (define square (a)(* a a))
        (check-expect (map square '(1 2 3 4)) '(1 4 9 16))

;; (filter f xs) applies a function to each element in a list and then returns 
;; the resulting list.

(define filter (p? xs)
    (foldr (lambda (x y) (if (p? x) (cons x y) y))'() xs))

        (check-expect (filter even? '(1 2 3 4)) '(2 4))
        (check-expect (filter even? '(1 3 5 7)) '())

;; (exists p? xs) takes a predicate p? and a list xs and returns true if there 
;; is at least one element in the list for which p? is true

(define exists? (p? xs)
    (foldr (lambda (x y) (|| (p? x) y )) #f xs))

        (check-assert (exists? even? '(1 2 3 4)))
        (check-assert(not (exists? even? '(1 3 5 7))))

;; (all p? xs) takes a predicate p? and a list xs and returns true if  
;; for every element in the list, p? is true

(define all? (p? xs)
    (foldr (lambda (x y) (&& (p? x) y )) #t xs))

        (check-assert (all? even? '(2 4 6 8)))
        (check-assert(not (all? even? '(1 3 5 7))))


;;
;; Problem 8
;;

;; (evens x) takes a value x and returns true if it is in the set of all even 
;; integers

;; laws:
;; (member? x evens) == (= (mod x 2) 0)
(val evens (lambda (x) (= (mod x 2) 0)))
        
        (define member? (x s) (s x))
        (check-assert (member? 24 evens))
        (check-assert (not(member? 159 evens)))


;; (two-digits x) takes a value x and returns true if it is in the set of all 
;; two-digit positive numbers

;; laws: 
;; (member? x two-digits) == (&& (<= 10 x) (> 100 x))
(val two-digits (lambda (x) (&& (<= 10 x) (> 100 x))))

        (check-assert (member? 24 two-digits))
        (check-assert (not(member? 159 two-digits)))


;; (add-element x s) takes a value x and a set s and returns a new set 
;; containing all the elements in set s and the element x

;; laws:
;; (member? x (add-element x s)) == #t
;; (member? x (add-element y s)) == #f, where (not (equal? y x))
(define add-element (x s)
    (lambda (i) (|| (s i) (equal? x i))))
    
        (check-assert (member? 25 (add-element 25 evens)))

;; (union s1 s2) returns the union of sets s1 and s2

;; laws:
;; (member? x (union s1 s2))  == (|| (s1 x) (s2 x))
(define union (s1 s2)
    (lambda (i) (|| (s1 i) (s2 i))))

        (check-assert (member? 99 (union evens two-digits)))
        (check-assert (member? 100 (union evens two-digits)))
        (check-assert (not(member? 101 (union evens two-digits))))


;; (inter s1 s2) returns the intersection of sets s1 and s2

;; laws:
;; (member? x (union s1 s2))  == (&& (s1 x) (s2 x))
(define inter (s1 s2)
    (lambda (i) (&& (s1 i) (s2 i))))

        (check-assert (member? 98 (inter evens two-digits)))
        (check-assert (not(member? 99 (inter evens two-digits))))
        (check-assert (not (member? 100 (inter evens two-digits))))
        (check-assert (not(member? 101 (inter evens two-digits))))


;; (diff s1 s2) returns the set of elements that are in s1 but not in s2

;; (member? x (union s1 s2))  == (&& (s1 x) (not(s2 x)))
(define diff (s1 s2)
    (lambda (i) (&& (s1 i) (not(s2 i)))))

        (check-assert (member? 100 (diff evens two-digits)))
        (check-assert (not (member? 98 (diff evens two-digits))))
        (check-assert (not(member? 99 (diff evens two-digits))))
        (check-assert (not(member? 101 (diff evens two-digits))))
