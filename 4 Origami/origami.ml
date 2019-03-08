(*
	Zadanie: Origami
	Autor: Mateusz Danowski
	Code Review: Rafał Czarnecki
*)

open List;;
type point = float * float
type kartka = point -> int

let eps = 0.000000001

(* Funkcje pomocnicze *)

let dlugosc (x, y) = (* Długość wektora *)
	sqrt(x *. x +. y *. y)

let dodaj (x1, y1) (x2, y2) = (* Suma dwóch wektorów *)
	(x1 +. x2, y1 +. y2)

let odejmij (x1, y1) (x2, y2) = (* Różnica dwóch wektorów *)
	(x1 -. x2, y1 -. y2)

let pomnoz c (x, y) = (* Pomnożenie wektora przez skalar [c] *)
	(c *. x, c *. y)

let podziel (x, y) c = (* Podzielenie wektora przez skalar [c], c != 0 *)
	(x /. c, y /. c)

let strona (x1, y1) (x2, y2) (px, py) = (* Mówi po której stronie wektora [x2 - x1, y2 - y1] jest punkt (px, py) *)
	let wektorowy = (x2 -. x1) *. (py -. y1) -. (px -. x1) *. (y2 -. y1) in (* Iloczyn wektorowy *)
	if abs_float (wektorowy) <= eps then 0 (* Punkt [p] leży na prostej wyznaczonej przez wektor *)
	else if wektorowy < 0. then -1 (* Punkt [p] leży po prawej stronie prostej wyznaczonej przez wektor *)
	else 1 (* Punkt [p] leży po lewej stronie prostej wyznaczonej przez wektor *)

let skalarny (x1, y1) (x2, y2) = (* Iloczyn skalarny dwóch wektorów *)
	(x1 *. x2) +. (y1 *. y2)

let symetria p a b = (* Odbija punkt [p] symetrycznie wzlędem prostej wyznaczonej przez punkty [a] i [b] *)
	let l = podziel (odejmij b a) (dlugosc (odejmij b a)) in
	let k = dodaj a (pomnoz (skalarny (odejmij p a) l) l) in
	odejmij (pomnoz 2. k) p

(* Konstruktory *)

let prostokat (x1, y1) (x2, y2) =  (* Zwraca kartkę, reprezentującą domknięty prostokąt o bokach równoległych do osi układu współrzędnych i lewym dolnym rogu [x1, y1] a prawym górnym [x2, y2] *)
	fun (x, y) ->
		if x1 -. eps <= x && x -. eps <= x2 && y1 -. eps <= y && y -. eps <= y2 (* Sprawdza czy punkt [x, y] jest wewnątrz prostokąta *)
		then 1
		else 0

let kolko p r  = (* Zwraca kartkę, reprezentującą kółko domknięte o środku w punkcie [p] i promieniu [r] *)
	fun x ->
		if (dlugosc (odejmij p x)) -. eps <= r (* Sprawdza czy punkt [x] jest wewnątrz kółka *)
		then 1
		else 0

(* Modyfikatory *)

let zloz p1 p2 k = (* Składa kartkę [k] wzdłuż prostej przechodzącej przez punkty [p1] i [p2] *)
	fun p ->
		let result = strona p1 p2 p in
		match result with
		| -1 -> 0
		| 0 -> k p
		| 1 -> k p + k (symetria p p1 p2)
		| _ -> assert (false)

let skladaj l k = (* Wynikiem jest złożenie kartki [k] kolejno wzdłuż wszystkich prostych z listy [l] *)
	fold_left (fun acc (p1, p2) -> zloz p1 p2 acc) k l
