;;
;;                     105 Assignment: Type Systems
;;                                Part B
;;
;;                     Valerie Zhang and Matt Torres
;;
;;

;; (drop n xs) given a natural number n and a list of values xs, returns xs without the first n elements 

(val drop 
  (type-lambda ['a]
    (letrec 
    [([drop-mono : (int (list 'a) -> (list 'a))]
        (lambda ([n : int] [xs : (list 'a)])
          (if ([@ null? 'a] xs)
              [@ '() 'a]
              (if (> n 0)
                (drop-mono (- n 1) ([@ cdr 'a] xs))
                xs))))]
    drop-mono)))

    ;; Unit Tests
    (check-expect ((@ drop int) 3 '(1 2 3 4)) '(4))
    (check-expect ((@ drop int) 3 '(1 2)) [@ '() int])
    ; (check-error ((@ drop bool) 3 '(1 2)))
  
    (check-type drop (forall ['a] (int (list 'a) -> (list 'a))))


;; (takewhile p? xs) given a predicate p? and a list xs, returns the largest 
;;                   prefix of xs in which every element satisfies p?.
(val takewhile 
  (type-lambda ('a)
    (letrec 
      [([takewhile-mono : (('a -> bool) (list 'a) -> (list 'a))]
        (lambda ([p? : ('a -> bool)] [xs : (list 'a)])
          (if (p? ([@ car 'a] xs))
            ([@ cons 'a] ([@ car 'a] xs) (takewhile-mono p? ([@ cdr 'a] xs)))
            [@ '() 'a])))]
    takewhile-mono)))

    ;; Unit Tests
    (check-type takewhile (forall ['a] (('a -> bool) (list 'a) -> (list 'a))))
    (check-expect ((@ takewhile int) (lambda ([x : int]) ([@ = int] (mod x 2) 0)) '(2 4 6 7 8 10 12)) '(2 4 6))
    (check-expect ((@ takewhile int) (lambda ([x : int]) ([@ = int] (mod x 2) 0)) '(1 4 6 7 8 10 12)) [@ '() int])