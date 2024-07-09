;;;;;;;;;;;;;;;;;;; COMP 105 SCHEME ASSIGNMENT ;;;;;;;;;;;;;;;

;; Author: Matthew Torres

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise 2

;; laws
;;  (sublist-match? '() ys) == #t
;;  (sublist-match? xs '()) == #f, where xs != '()
;;  (sublist-match? xs ys) == (sublist-match? (cdr xs) (cdr ys)), where 
;;                                      (car xs) == (car ys)
;;  (sublist-match? xs ys) == #f, where (car xs) != (car ys)

;; (sublist-match? xs ys) returns #t if two sublists xs and ys have a matching 
;; length n sublist, beginning at the first element of each sublist, where n is 
;; the length of xs

(define sublist-match? (xs ys)
    (if (= xs '())
        #t
        (if (= ys '())
            #f
            (if (equal? (car xs) (car ys))
                (sublist-match? (cdr xs) (cdr ys))
                #f))))
       
        ;; check-assert tests
        (check-assert (sublist-match? '(1 2 3) '(1 2 3)))
        (check-assert (not (sublist-match? '(1 2 3) '(1 0 2 3))))
        (check-assert (not (sublist-match? '(1 2 3) '())))
        (check-assert (sublist-match? '() '(1 2 3)))


;; (contig-sublist? xs ys) when both xs and ys are in set LIST(ATOM),
;; returns true when xs is a contiguous subsequence of ys.

;; laws (not attempted because optional)
(define contig-sublist? (xs ys)

    (if (= ys '()) 
        #f 
        (if (= (sublist-match? xs ys) #t)
            #t
            (contig-sublist? xs (cdr ys)))))
        
        ;; check-assert tests
        (check-assert (contig-sublist? '(1 2 3) '(0 1 2 3 4)))
        (check-assert (not (contig-sublist? '(1 2 3) '(0 1 0 2 3 4))))

        (check-assert (contig-sublist? '('a 'a 'b) '(0 'a 2 'a 4 'a 'a 'a 'b)))
        (check-assert (not (contig-sublist? '('a 'b 'c) '(0 'a 0 'b 3 'c))))
        
        (check-assert (not (contig-sublist? '(1 2 3) '(0 1 2))))
        (check-assert (contig-sublist? '() '(0 1 2)))
        (check-assert (not (contig-sublist? '(0 1 2) '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise 3


;; (flatten xs) consumes a list of S‐expressions and erases internal  brackets.

;; laws:
;;   (flatten '()) == ()
;;   (flatten xs) == (cons (flatten (car xs)) (flatten (cdr xs))),
;;                      when (pair? (car xs)) is #t
;;   (flatten xs) == (cons (car xs) (flatten (cdr xs))),
;;                      when (pair? (car xs)) is #f

(define flatten (xs)
        
    (if (null? xs)
        '()
        (if (pair? (car xs))
            (append (flatten (car xs)) (flatten (cdr xs)))
            (cons (car xs) (flatten (cdr xs))))))

        ;; check-expect tests
        (check-expect (flatten '()) '())
        (check-expect (flatten '(1 2 3)) '(1 2 3))
        (check-expect (flatten '(1 2 (3 4))) '(1 2 3 4))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise 4


;; (take n xs)  expects a natural number and a list of values. It returns a 
;; list containing the first n elements of xs, or all of xs if n is greater 
;; than the length of xs.

;; laws:
;;   (take '()) == '()
;;   (take xs) == (cons (car xs) (take (- n 1) (cdr xs))), where length xs > 0
;;   (take xs) == '(), where length of xs = 0

(define take (n xs)
    (if (null? xs) 
        '()  
        (if (> n 0)
            (cons (car xs) (take (- n 1) (cdr xs)))
            '())))

        ;; check-expect tests
        (check-expect (take 3 '(1 2 3 4)) '(1 2 3))
        (check-expect (take 3 '(1 2)) '(1 2))


;; (drop n xs) expects a natural number and a list of values. It returns xs 
;; without the first n elements, or the empty list if n is greater than the 
;; length of xs.

;; laws:
;;   (drop n '()) == '()
;;   (drop 0 xs) == xs
;;   (drop n xs) == (drop (- n 1) (cdr xs)), where n > 0


(define drop (n xs)
    (if (null? xs) 
        '()  
        (if (> n 0)
            (drop (- n 1) (cdr xs))
            xs )))

        ;; check-expect tests
        (check-expect (drop 3 '(1 2 3 4)) '(4))
        (check-expect (drop 3 '(1 2)) '())



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise 5


;; (zip xs ys) converts a pair of lists to a list of pairs by associating 
;; corresponding values in the two lists.

;; laws:
;;   (zip '() ys) == '()
;;   (zip  xs '()) == '()
;;   (zip xs ys) == (cons (list2 (car xs) (car ys)) (zip (cdr xs) (cdr ys)))

(define zip (xs ys)
    (if (null? xs) 
        '()  
        (if (null? ys) 
            '()
            (cons (list2 (car xs) (car ys)) (zip (cdr xs) (cdr ys))))))

        ;; check-expect tests
        (check-expect (zip '(1 2 3) '(A B C)) '((1 A) (2 B) (3 C)))
        (check-expect (zip '(1 2) '(A B C)) '((1 A) (2 B)))
        (check-expect (zip '(1 2 3) '(A B)) '((1 A) (2 B)))
        (check-expect (zip '() '(A B)) '())
        (check-expect (zip '(A B) '()) '())



;;   (collect-last-elts ps) returns a single list composed of all the second 
;; elements of each pair in a list of pairs ps

;; laws:
;;   (collect-last-elts '()) == '()
;;   (collect-last-elts ps) == '(), where (car ps) is '()
;;   (collect-last-elts ps) == (cons (car (cdr (car ps))) 
;;                              (collect-last-elts (cdr ps))))))

(define collect-last-elts (ps)
    (if (null? ps) 
        '()  
        (if (null? (car ps))
            '()
            (cons (car (cdr (car ps))) (collect-last-elts (cdr ps))))))

        ;; Check-expect tests
        (check-expect (collect-last-elts '((1 2) (3 4) (5 6))) '(2 4 6))
        (check-expect (collect-last-elts '(() ())) '())



;;   (collect-first-elts ps) returns a single list composed of the first 
;; elements of each pair in a list of pairs ps


;; laws:
;;   (collect-first-elts '()) == '()
;;   (collect-first-elts ps) == '(), where (car ps) is '()
;;   (collect-first-elts ps) == (cons (car (car ps)) 
;;                                (collect-first-elts (cdr ps))))))

(define collect-first-elts (ps)
    (if (null? ps) 
        '()  
        (if (null? (car ps))
            '()
            (cons (car (car ps)) (collect-first-elts (cdr ps))))))
        
        ;; Check-expect tests
        (check-expect (collect-first-elts '((1 2) (3 4) (5 6))) '(1 3 5))
        (check-expect (collect-first-elts '(() ())) '())




;; (unzip ps) converts a list of pairs to a pair of lists containing the 
;; separate elements.

;; laws: (not attempted because optional)

(define unzip (ps)
    (if (null? ps) 
        '()  
        (cons (collect-first-elts ps) (cons (collect-last-elts ps) '()))))

        ;; check-expect tests
        (check-expect (unzip '((1 2) (3 4) (5 6) (7 8))) '((1 3 5 7) (2 4 6 8)))
        (check-expect (unzip '()) '())
        (check-expect (unzip '(() ())) '(() ()))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise 6


;; (get-original-value xs ys maxval) takes equal sized nonempty lists xs and 
;; ys, and a number maxval, and returns the value in xs which corresponds to 
;; the value in ys which equals maxval.

;; laws:
;;   (get-original-value xs ys maxval ) == (car xs), where (car ys) = maxval
;;   (get-original-value xs ys maxval ) == 
;;      (get-original-value (cdr xs) (cdr ys) maxval)

(define get-original-value (xs ys maxval)
    (if (= (car ys) maxval)
        (car xs)
        (get-original-value (cdr xs) (cdr ys) maxval)))

        ;; check-expect test
        (check-expect (get-original-value '(1 2 3 4 5) '(2 4 6 8 10) 10) 5)

;; (arg-max f xs) takes a function f and a nonempty list xs, returns the value 
;; x in xs that maximizes the number (f x)

;; laws:
;;   (arg-max f xs) == (get-original-value xs (map f xs) (max* (map f xs)))

(define arg-max (f xs)
    (get-original-value xs (map f xs) (max* (map f xs))))

        ;; check-expect tests
        (define square (a) (* a a))
        (check-expect (arg-max square '(5 4 3 2 1)) 5)
        (check-error (arg-max square '()))
        (check-expect (arg-max car '((105 PL) (160 Algorithms) (170 Theory)))
            '(170 Theory))