(* 
    Zadanie: Modyfikacja drzew
    Autor: Mateusz Danowski
    Code Review: Dawid Jamka
*)

(* lewe poddrzewo, przedział trzymany w wierzchołku, prawe poddrzewo, wysokość, wielkość poddrzewa *)
type interval = int * int
type t =
    | Node of t * interval * t * int * int
    | Empty

(* Funkcje pomocnicze *)

let height s = (* Odległość do najdalszego liścia od korzenia s *)
    match s with
    | Node (_, _, _, h, _) -> h
    | Empty -> 0

let subSize s = (* Liczba pojedynczych wartośći przechowywanych przez s *)
    match s with
    | Node (_, _, _, _, s) -> s
    | Empty -> 0

let plus a b = (* Bezpieczne dodawanie, zakłada a, b > 0 *)
    if a + b < 0 then max_int
    else a + b

let cnt (x, y) = (* Liczba wartości w przedziale [x, y] *)
    if y - x + 1 <= 0 then max_int
    else y - x + 1

let make l k r =
    Node (l, k, r, max (height l) (height r) + 1, plus (plus (subSize l) (subSize r)) (cnt k))

let bal l k r =
    let hl = height l in
    let hr = height r in
    if hl > hr + 2 then
        match l with
        | Node (ll, lk, lr, _, _) ->
            if height ll >= height lr then make ll lk (make lr k r)
            else
                (match lr with
                | Node (lrl, lrk, lrr, _, _) ->
                    make (make ll lk lrl) lrk (make lrr k r)
                | Empty -> assert false)
        | Empty -> assert false
    else if hr > hl + 2 then
        match r with
        | Node (rl, rk, rr, _, _) ->
            if height rr >= height rl then make (make l k rl) rk rr
            else
                (match rl with
                | Node (rll, rlk, rlr, _, _) ->
                    make (make l k rll) rlk (make rlr rk rr)
                | Empty -> assert false)
        | Empty -> assert false
    else make l k r

let rec min_elem s = (* Zwraca najmniejszy przedział s *)
    match s with
    | Node (Empty, k, _, _, _) -> k
    | Node (l, _, _, _, _) -> min_elem l
    | Empty -> raise Not_found

let rec remove_min_elem s = (* Usuwa najmniejszy przedział s *)
    match s with
    | Node (Empty, _, r, _, _) -> r
    | Node (l, k, r, _, _) -> bal (remove_min_elem l) k r
    | Empty -> invalid_arg "ISet.remove_min_elem"

let rec max_elem s = (* Zwraca największy przedział s *)
    match s with
    | Node (_, k, Empty, _, _) -> k
    | Node (_, _, r, _, _) -> max_elem r
    | Empty -> raise Not_found

let rec remove_max_elem s = (* Usuwa największy przedział s *)
    match s with
    | Node (l, _, Empty, _, _) -> l
    | Node (l, k, r, _, _) -> bal l k (remove_max_elem r)
    | Empty -> invalid_arg "ISet.remove_max_elem"

let merge t1 t2 = (* Łączy dwa drzewa AVL w jedno *)
    match t1, t2 with
    | Empty, _ -> t2
    | _, Empty -> t1
    | _ ->
        let k = min_elem t2 in
        bal t1 k (remove_min_elem t2)

(* Funkcje z iSet.mli *)

let empty = Empty

let is_empty s =
    s = Empty

let mem n s = (* Czy s zawiera n *)
    let rec loop acc s =
        match s with
        | Node (l, (x, y), r, _, _) ->
            if n < x then loop acc l
            else if n > y then loop acc r
            else true
        | Empty -> false in
    loop false s

let rec iter f s = (* Wykonuje funkcję f na wierzchołkach drzewa w kolejności infixowej *)
    match s with
    | Node (l, k, r, _, _) -> iter f l; f k; iter f r
    | Empty -> ()

let rec fold f s acc = (* Wykonuje funkcję f na wierzchołkach drzewa w kolejności infixowej, przy czym puste drzewo zwraca acc *)
    match s with
    | Node (l, k, r, _, _) -> fold f r (f k (fold f l acc))
    | Empty -> acc

let elements s = (* Zwraca listę wszystkich przedziałów w kolejności rosnącej *)
    let rec loop acc s =
        match s with
        | Node (l, k, r, _, _) -> loop (k :: loop acc r) l
        | Empty -> acc in
    loop [] s

let below n s = (* Zwraca liczbę wartości w drzewie mniejszych bądź równych n *)
    let rec loop acc s =
        match s with
        | Node (l, (x, y), r, _, _) ->
            if n < x then loop acc l
            else if n > y then loop (plus acc (plus (cnt (x, y)) (subSize l))) r
            else plus acc (plus (cnt (x, n)) (subSize l))
        | Empty -> acc in
    loop 0 s

(* Dzieli drzewo AVL s według wartości n, zwraca krotkę (l, b, r), gdzie
   l - drzewo AVL z wartościami mniejszymi od n
   b - czy n należy do s
   r - drzewo AVL z wartościami większymi od n
*)
let split n s =
    let rec loop l_acc pres r_acc s =
        match s with
        | Node (l, (x, y), r, _, _) ->
            if x <= n && n <= y then
                let l_new =
                    if x < n then bal l (x, min y (n - 1)) l_acc
                    else merge l l_acc in
                let r_new =
                    if y > n then bal r_acc (max x (n + 1), y) r
                    else merge r r_acc in
                (l_new, true, r_new)
            else if n < x then
                let (l_acc, pres, r_acc) = loop l_acc pres r_acc l in
                (l_acc, pres, bal r_acc (x, y) r)
            else
                let (l_acc, pres, r_acc) = loop l_acc pres r_acc r in
                (bal l (x, y) l_acc, pres, r_acc)
        | Empty -> (l_acc, pres, r_acc) in 
    loop Empty false Empty s

let add (x, y) s = (* Zwraca drzewo AVL s z dodanym przedziałem [x, y] *)
    let (l, _, nr) = split x s in
    let (_, _, r) = split y nr in
    let ((x, _), l) = 
        if l != Empty && mem (x - 1) l then
            (max_elem l, remove_max_elem l)
        else ((x, y), l) in
    let ((_, y), r) =
        if r != Empty && mem (y + 1) r then
            (min_elem r, remove_min_elem r)
        else ((x, y), r) in
    bal l (x, y) r

let remove (x, y) s = (* Zwraca drzewo AVL z usuniętym przedziałem [x, y] *)
    let (l, _, nr) = split x s in
    let (_, _, r) = split y nr in
    merge l r
