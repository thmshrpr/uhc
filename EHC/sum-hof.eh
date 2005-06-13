let data Ordering = LT | EQ | GT
    data Bool = False | True
    data List a = Nil | Cons a (List a)
in
let foreign import jazy "primAddInt" add :: Int -> Int -> Int
    foreign import jazy "primCmpInt" compare :: Int -> Int -> Ordering
    gt  = \x -> \y ->
            case compare x y of
                GT -> True
                _  -> False
in
let upto = \m n ->
        if gt m n then
            Nil
        else
            Cons m (upto (add m 1) n)
    map = \f l ->
        case l of
            Nil       -> Nil
            Cons n ns -> Cons (f n) (map f ns)
    sum = \l ->
        case l of
            Nil       -> 0
            Cons n ns -> add n (sum ns)
in sum (map (add 1) (upto 0 9))
