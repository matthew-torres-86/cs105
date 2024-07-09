(val x 10)
(val f (lambda () x))
(val x 5)
(if (= (f) 5)
    'uscheme_semantics
    'new_semantics
)