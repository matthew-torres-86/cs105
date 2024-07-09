;; CS 105 Homework 5
;; Matthew Torres

;; 
;; Problem 2
;;

;; laws:
;; (list-of? A? '()) == #t
;; (list-of? A? v) == #f, where (not (pair? v)) => #t
;; (list-of? A? v) == #f, where (not (A? (car v))) => #t, and (pair? v) => #t
;; (list-of? A? v) == (list-of? A? (cdr v)), where (pair? v) => #t and 
;;                                                      (A? (car v)) => #t

;; (list-of? A? v) returns #t if v is a list of values, each of which satisfies 
;; A?. Otherwise, (list-of? A? v) returns #f
(define list-of? (A? v)
    (if (equal? v '())
        #t
        (if (pair? v)
            (if (A? (car v))
                (list-of? A? (cdr v))
                #f)
            #f)))

(define value? (_) #t)
(define even? (x) (= (mod x 2) 0))

        ;; unit-tests
        (check-assert (list-of? value? '()))
        (check-assert (list-of? value? '(1 'a () (1 2 3) #f)))
        (check-assert (not (list-of? value? #t)))
        (check-assert (list-of? value? '(all?)))

        (check-assert (list-of? pair? '((12) (1 2 3))))
        (check-error (list-of? even? '('a () (1 2 3) #f)))


;; 
;; Problem 3
;;

(record not [arg])
(record or  [args])
(record and [args])

;; laws:
;; (formula? f) == #t, where (symbol? f) => #t
;; (formula? f) == (formula? (not-arg f)), where (not? f) => #t
;; (formula? f) == (list-of? formula? (or-args f)), where (or? f) => #t
;; (formula? f) == (list-of? formula? (and-args f)), where (and? f) => #t
;; (formula? f) == #f, where (symbol? f) => #f and (not? f) => #f and
;; (or? f) => #f and (and? f) => #f

;; (formula? f) given an arbitrary μScheme value f, returns #t if f represents 
;; a Boolean formula and #f otherwise

(define formula? (f)
    (if (symbol? f)
        #t
        (if (not? f)
            (formula? (not-arg f))
            (if (or? f)
                (list-of? formula? (or-args f))
                    (if (and? f)
                        (list-of? formula? (and-args f))
                        #f )))))

        ;; unit-tests
        (check-assert (formula? 'x))
        (check-assert (formula? (make-not 'x)))
        (check-assert (formula? (make-or '(x y))))
        (check-assert (formula? (make-or '())))
        (check-assert (formula? (make-and '(x y))))
        (check-assert (formula? (make-and '())))
        (check-assert (formula? (make-and 
                                 (list2 (make-or (list2 'x (make-not 'x)))      
                                        (make-not 'x)))))
        (check-assert (not (formula? 1)))
        (check-assert (not (formula? '(1 2 3))))


;; 
;; Problem 4
;;

;; laws:

;; (eval-formula f env) takes two arguments: a formula f and an environment
;; env. The environment is an association list in which each key is a symbol 
;; (representing a Boolean variable) and each value is a Boolean literal (i.e., 
;; #t or #f). If the formula f is satisfied in the given environment, 
;; (eval-formula f env) returns #t; otherwise, it returns #f

(define eval-formula (f env)
    (if (symbol? f)
        (find f env)
        (if (not? f)
            (not (eval-formula (not-arg f) env))
            (if (or? f)
                (if (null? (or-args f))
                    #t
                    (or (eval-formula (car (or-args f)) env)
                        (eval-formula (car (cdr (or-args f))) env)))
                (if (and? f)
                    (if (null? (and-args f))
                        #t
                        (and (eval-formula (car (and-args f)) env)
                            (eval-formula (car (cdr (and-args f))) env)))
                        (null? f))))))

        (check-assert(eval-formula 'x '((x #t))))
        (check-assert(eval-formula (make-not 'x) '((x #f))))
        (check-assert(eval-formula (make-and '(x y)) '((x #t) (y #t))))
        (check-assert(eval-formula (make-and '()) '((x #t) (y #t))))
        (check-assert(eval-formula (make-or '(x y)) '((x #f) (y #t))))
        (check-assert(eval-formula (make-or '()) '((x #t) (y #t))))
        (check-assert(not (eval-formula 
                            (make-and (list2 
                                        (make-or (list2 'x (make-not 'x))) 
                                        (make-not 'x))) '((x #t)))))



;; 
;; Problem 5
;;

;; (solve-formula f bool cur fail succeed) extends assignment cur to find an 
;; assignment that makes the single f equal to bool.

;; solve-formula laws:
;; (solve-formula x          bool cur fail succeed) == 
;;      (solve-symbol x bool cur fail succeed), where x is a symbol
;; (solve-formula (make-not f)  bool cur fail succeed) == 
;;      (not (solve-formula (not-arg f) bool cur fail succeed))
;; (solve-formula (make-or  fs) #t   cur fail succeed) == 
;;      (solve-any fs #t cur fail succeed)
;; (solve-formula (make-or  fs) #f   cur fail succeed) == 
;;      (solve-any fs #f cur fail succeed)
;; (solve-formula (make-and fs) #t   cur fail succeed) == 
;;      (solve-all fs #t cur fail succeed)
;; (solve-formula (make-and fs) #f   cur fail succeed) == 
;;      (solve-all fs #f cur fail succeed)


;; (solve-all f bool cur fail succeed) extends cur to find an assignment that 
;; makes every formula in the list fs equal to bool.

;; solve-all laws:
;; (solve-all '()         bool cur fail succeed) == 
;;      (succeed cur fail)
;; (solve-all (cons f fs) bool cur fail succeed) == 
;;      (solve-formula f bool cur fail (lambda (cur res) (solve-all    fs bool 
;; cur res succeed)))


;; (solve-any fs bool cur fail succeed) extends cur to find an assignment that 
;; makes any one of the fs equal to bool.

;; solve-any laws:
;; (solve-any '()         bool cur fail succeed) == 
;;      (fail)
;; (solve-any (cons f fs) bool cur fail succeed) == 
;;      (solve-formula f bool cur 
;;               (lambda () (solve-any fs bool cur fail     succeed)) succeed) 


;; (solve-symbol x bool cur fail succeed) If x is bound to bool in cur, 
;; succeeds with environment cur and resume continuation fail.
;; If x is bound to (not bool) in cur, fails
;; If x is not bound in cur, extends cur with a binding of x to bool, then 
;; succeeds with the extended environment and continuation fail

;; solve-symbol laws: 
;; (solve-symbol x bool cur fail succeed) == 
;;      (succeed cur fail), where x is bool in cur
;; (solve-symbol x bool cur fail succeed) == 
;;      (fail), where x is (not bool) in cur
;; (solve-symbol x bool cur fail succeed) == 
;;      (succeed (bind x bool cur) fail), where x is not bound in cur

;; (solve-sat f fail succ) searches for an assignment that satisfies formula f. 
;; If it finds a satisfying assignment, it calls (succ env resume) with a 
;; satisfying assignment env (an association list) and a resume continuation 
;; resume (which does not take any arguments). Otherwise, it calls (fail) with 
;; no arguments.

(define solve-sat (f fail succ)
    (letrec
     (
        [solve-formula
            (lambda (f bool cur fail succeed) 
                (if (symbol? f)
                    (solve-symbol f bool cur fail succeed)
                    (if (not? f)
                        (solve-formula (not-arg f) (not bool) cur fail succeed)
                        (if (or? f)
                            (solve-any (or-args f) bool cur fail succeed)
                            (if (and? f)
                                (solve-all (and-args f) bool cur fail succeed)
                                cur)))))]

        [solve-all
            (lambda (fs bool cur fail succeed) 
                (if (null? fs)
                    (succeed cur fail)
                    (solve-formula (car fs) bool cur fail 
                        (lambda (cur res)(solve-all (cdr fs) bool cur res 
                                            succeed)))))]

        [solve-any
            (lambda (fs bool cur fail succeed) 
                (if (null? fs)
                    (fail)
                    (solve-formula (car fs) bool cur 
                        (lambda () 
                            (solve-any (cdr fs) bool cur fail succeed)) 
                            succeed)))]
        
        [solve-symbol
            (lambda (f bool cur fail succeed) 
                (if (null? (find f cur))
                    (succeed (bind f bool cur) fail)
                    (if (= bool (find f cur))
                        (succeed cur fail)
                        (fail))))]
        ) (solve-formula f #t '() fail succ))
    )

        ;; unit tests
        (check-assert (function? solve-sat))    ; correct name
        (check-error (solve-sat))                ; not 0 arguments
        (check-error (solve-sat 'x))             ; not 1 argument
        (check-error (solve-sat 'x (lambda () 'fail)))   ; not 2 args
        (check-error
            (solve-sat 'x (lambda () 'fail) (lambda (c r) 'succeed) 'z)) 
        (check-error (solve-sat 'x (lambda () 'fail) (lambda () 'succeed)))
        (check-error (solve-sat 'x (lambda () 'fail) (lambda (_) 'succeed)))

        (check-error (solve-sat
                       (make-and (list2 'x (make-not 'x)))
                       (lambda (_) 'fail)
                       (lambda (_) 'succeed)))

        (check-expect
            (solve-sat 'x
                  (lambda () 'fail)
                  (lambda (cur resume) 'succeed))
            'succeed)

    (check-expect   ; x is solved by '((x #t))
            (solve-sat 'x
                  (lambda () 'fail)
                  (lambda (cur resume) (find 'x cur)))
            #t)

    (check-expect   ; (make-not 'x) can be solved
            (solve-sat (make-not 'x)
                  (lambda () 'fail)
                  (lambda (cur resume) 'succeed))
            'succeed)

    (check-expect   ; (make-not 'x) is solved by '((x #f))
       (solve-sat (make-not 'x)
                  (lambda () 'fail)
                  (lambda (cur resume) (find 'x cur)))
            #f)

    (check-expect   ; (make-and (list2 'x (make-not 'x))) cannot be solved
       (solve-sat (make-and (list2 'x (make-not 'x)))
                  (lambda () 'fail)
                  (lambda (cur resume) 'succeed))
            'fail)

        (check-expect (solve-sat(make-and '(x y)) (lambda () 'fail) 
                                (lambda (cur res) 'success)) 'success)
        (check-expect (solve-sat(make-or '(x y)) (lambda () 'fail) 
                                (lambda (cur res) 'success)) 'success)

        (check-expect (solve-sat
                        (make-and 
                            (list2 
                                (make-or (list2 'x (make-not 'x)))
                                (make-not 'x)))
                        (lambda () 'fail) 
                        (lambda (cur res) 'success))'success)

        (check-expect 
            (solve-sat
                (make-and 
                    (list3 (make-or (list3 'x 'y 'z)) 
                            (make-and (list2 (make-not 'x) 'x))
                            (make-or (list3 'x (make-not 'y) (make-not 'z)))))
                        (lambda () 'fail) 
                                (lambda (cur res) 'success))'fail)