;; step 5
(check-type 3 int)
(check-type #t bool)
(check-type 'hello sym)

;; step 6
(check-type (if #t 1 0) int)
(check-type (if #t 'yes 'no) sym)
(check-type (if #t #t #f) bool)

(check-type-error (if #t 1 #f))
(check-type-error (if #t 'lol #f))
(check-type-error (if #t 'lol 1))
(check-type-error (if 1 1 1))

;; step 7
(check-type + (int int -> int))
(check-type > (int int -> bool))

(check-type-error p)

;; step 9
(val x 3)
(val y #t)
(val z 'help)

(check-type x int)
(check-type y bool)
(check-type z sym)
(check-type-error p)

;; step 10
(check-type (+ x 1) int)
(check-type (> x 1) bool)
; (check-type ((@ = int) 1 1 ) bool)
(check-type-error (= 1 1))
(check-type-error (p 1 #f))

;; step 11
(check-type (let ([x 5] [y 3]) (+ x y)) int)
(check-type (let ([x 1] [y 2]) (< x y)) bool)
;; (check-type-error (let ([x p] [y 1]) (and x y))
(check-type-error (let ([x #t] [y #f]) (+ x y)))

;; step 12
(check-type (lambda ([x : int] [y : int]) (> x y)) (int int -> bool))
(check-type (lambda ([x : int] [y : int]) (+ x y)) (int int -> int))
(check-type-error (lambda ([x : bool] [y : int]) (+ x y)))


;; step 13
(check-type (while #t 1) unit)
(check-type-error (while #t p))
(check-type-error (while 1 #t))
; (check-type-error (while ))

(check-type (set x 1) int)
(check-type (set y #f) bool)
(check-type-error (set x #t))
(check-type-error (set a #t))

(check-type (begin ) unit)
(check-type (begin (+ x 2) (+ x 3)) int)
(check-type (begin (> x 1) (< 2 1)) bool)

; (check-type-error (begin ))
; (check-type-error (begin ))


;; step 14
(check-type (let* ([x 1] [y 2]) (+ x y)) int)
(check-type (let* ([x 1] [y 2]) (> x y)) bool)
(check-type (let* [] (+ 1 2)) int)
(check-type-error (let* ([x p] [y 1]) (+ x y)))
(check-type-error (let* [] p))

;; step 15

(check-type (letrec [([fa :  (int -> int)] (lambda ([x : int]) (fb x))) 
                      ([fb :  (int -> int)] (lambda ([x : int]) (+ x 1)))]  
                    (fa 0)) int)

(check-type (letrec [([fa :  (int -> int)] (lambda ([x : int]) (fb x))) 
                      ([fb :  (int -> int)] (lambda ([x : int]) (+ x 1)))]  
                    (> (fa 1) 1)) bool)
; (check-type-error (letrec (([x : (-> bool)] (lambda () (> 1 2))) ([y : (-> int)] (lambda () (- 1 2)))) (> x y)))
; (check-type-error (letrec (([x : int] 1) ([y : int] (lambda () (- 1 2)))) (> x y)))
; (check-type-error (letrec (([x : int] (lambda () (> 1 2))) ([y : int] (lambda () (- 1 2)))) (> x y)))


;; step 16
(val-rec [m : int] (lambda () (+ 2 1)))
(val-rec [n : bool] (lambda () (> x 2)))

; (check-type m int)
; (check-type n bool)
; (check-type-error (val-rec [p : int] (+ x 1)))
; (check-type-error (val-rec [p : bool] (> x 1)))

(define int lol ([x : int] [y : int]) (+ x y))
(define bool lamow ([x : int] [y : int]) (> x y))

(check-type lol (int int -> int))
(check-type lamow (int int -> bool))
(check-type-error (define int wrong ([p : bool] [x : int]) (+ x p)))
(check-type-error (define bool wrong2 ([p : bool] [x : int]) (> x p)))

; ;; step 17
