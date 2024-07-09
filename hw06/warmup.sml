(***** Problem 1 *****)
(* mynull l tells whether the list l is empty *)
fun mynull [] = true
  | mynull (x :: xs) = false

        (* Unit Test *)
        val () =
            Unit.checkAssert "[] is null"
            (fn () => mynull [])

(***** Problem 2 *****)
(***** a *****)
fun reverse xs = foldl op :: [] xs
         
        (* (int list) string buffer for unit testing *)
        val int_list_toString = Unit.listString Unit.intString
        val string_list_toString = Unit.listString Unit.stringString
        (* Unit Test *)
        val () =
                Unit.checkExpectWith int_list_toString "reversing empty"
                (fn () => reverse [])
                []
        (* Unit Test *)
        val () =
                Unit.checkExpectWith int_list_toString "reversing 1 2 3"
                (fn () => reverse [1, 2, 3])
                [3, 2, 1]

(***** b *****)
(* minlist xs returns the smallest element of a nonempty list of integers*)
fun minlist [] = raise Match
  | minlist (x::xs) = foldr Int.min x xs

        (* Unit Test *)
        val () =
                Unit.checkExpectWith Unit.intString "smallest of singleton"
                (fn () => minlist [1])
                1
        val () =
                Unit.checkExpectWith Unit.intString "smallest of list"
                (fn () => minlist [1, 5, 17, ~43, 25])
                ~43


(***** Problem 3 *****)
exception Mismatch

(* zip (xs, ys) takes a pair of lists (of equal length) and returns the 
 * equivalent list of pairs. If the lengths don’t match, raise the exception 
 * Mismatch *)
fun zip ([], []) = []
  | zip (x::xs, []) = raise Mismatch 
  | zip ([], y::ys) = raise Mismatch 
  | zip (x::xs, y::ys) = (x, y) :: zip (xs, ys) 

        (* Some new string builders *)
        val int_pair_toString = Unit.pairString Unit.intString Unit.intString
        val list_pair_toString = Unit.listString int_pair_toString
        (* Unit Test *)
        val () =
                Unit.checkExpectWith list_pair_toString "zip on integer lists"
                (fn () => zip ([1, ~2, 4], [9, 12, 0]))
                [(1, 9), (~2, 12), (4, 0)]


(***** Problem 4 *****)
(* pairfoldrEq f acc xs ys behaves like foldr, but walks a three-argument 
 * function over a pair of lists of equal length
*)
fun pairfoldrEq f acc [] [] = acc
  | pairfoldrEq f acc (x::xs) [] = raise Mismatch
  | pairfoldrEq f acc [] (y::ys) = raise Mismatch
  | pairfoldrEq f acc (x::xs) (y::ys) = pairfoldrEq f (f (x, y, acc)) xs ys

val int_pair_toString = Unit.pairString Unit.intString Unit.intString
val bool_pair_toString = Unit.pairString Unit.boolString Unit.boolString
val list_pair_toString = Unit.listString int_pair_toString
val list_boolpair_toString = Unit.listString bool_pair_toString

(* zip2 (x, y, acc) forms a pair (x, y) and appends it to list acc *)
fun zip2 (x, y, acc) = acc @ [(x, y)] 

(* ziptoo (xs, ys) behaves like zip above *)
fun ziptoo (xs, ys) = pairfoldrEq zip2 [] xs ys

    val () =
        Unit.checkExpectWith list_pair_toString "ziptoo on integer lists"
        (fn () => ziptoo ([1, ~2, 4], [9, 12, 0]))
        [(1, 9), (~2, 12), (4, 0)]



(***** Problem 5 *****)
(* concat xs takes a list of lists of 'a and produces a single list of 'a 
 * containing all the elements in the correct order *)
fun concat [] = []
  | concat (x::ys) = (foldr op :: [] x) @ (concat ys)

val () =
        Unit.checkExpectWith int_list_toString "concat on integer lists"
        (fn () => concat [[1], [2, 3, 4], [], [5, 6]])
        [1, 2, 3, 4, 5, 6]


(***** Problem 6 *****)
datatype ordsx 
  = BOOL of bool
  | NUM  of int
  | SYM  of string
  | SXS  of ordsx list

fun sxString (SYM s)   = s
  | sxString (NUM n)   = Unit.intString n
  | sxString (BOOL b)  = if b then "true" else "false"
  | sxString (SXS sxs) = "(" ^ String.concatWith " " (map sxString sxs) ^ ")"


(* numbersSx xs converts a list of numbers into an ordinary S-expression *)
fun numbersSx xs = SXS (map NUM xs) 

val () = 
Unit.checkExpectWith sxString  "ordsx list of numbers"
        (fn () => numbersSx [1, 2, 3])
        (SXS [NUM 1, NUM 2, NUM 3])

(* flattenSyms ordsx extracts just the symbols from an ordinary S-expression *)
fun flattenSyms (SYM s) = s :: []
  | flattenSyms (NUM _) = []
  | flattenSyms (BOOL _) = []
  | flattenSyms (SXS (x::xs)) = (flattenSyms x) @ (flattenSyms (SXS xs))
  | flattenSyms (SXS []) = []


val () = 
Unit.checkExpectWith string_list_toString "flatten list"
    (fn () => flattenSyms (SXS [NUM 1, SYM "hi", 
                                SXS [NUM 2, SYM "hello"]]))
        ["hi", "hello"]

(* end *)
val () = Unit.reportWhenFailures ()
        