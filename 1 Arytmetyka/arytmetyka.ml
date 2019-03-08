(* 
	Zadanie: Arytmetyka przybliżonych wartości
	Autor: Mateusz Danowski
	Code Review: Hugo Dutka
*)

type przedzial = float * float 
(* reprezentacja jako przedział (l, r), para przedziałów (l1, r1) U (l2, r2) albo nan *)
type wartosc = Odcinek of przedzial | Para of (przedzial * przedzial) | Puste

(* FUNKCJE POMOCNICZE *)

let abs x = (* Zwraca |x| *)
	if (x >= 0.) then x
	else (-1.) *. x

let przeciwne x = (* Zwraca -x *)
	(-1.) *. x

let czyNAN x = (* Zwraca true gdy x jest równe nan *)
	compare x nan = 0

let mnoz (l, r) = (* Rozwiązuje problem mnożenia zera i nieskończoności *)
	if (l = 0. && r = infinity)
	|| (l = 0. && r = neg_infinity)
	|| (l = infinity && r = 0.)
	|| (l = neg_infinity && r = 0.) then 0.
	else l *. r

let pomnoz (la, ra) (lb, rb) = (* Mnoży dwa przedziały *)
	min (min (mnoz (la, lb)) (mnoz (la, rb))) (min (mnoz (ra, lb)) (mnoz (ra, rb))),
	max (max (mnoz (la, lb)) (mnoz (la, rb))) (max (mnoz (ra, lb)) (mnoz (ra, rb)))

let polacz x = (* Otrzymuje parę przedziałów i próbuje je scalić w jeden *)
	match x with
	| Odcinek (l, r) -> Odcinek (l, r) (* nie zachodzi *)
	| Para ((l1, r1), (l2, r2)) -> (* l1 <= l2 zawsze zachodzi *)
		if (r1 >= l2) then Odcinek (l1, r2)
		else Para ((l1, r1), (l2, r2))
	| Puste -> Puste (* nie zachodzi *)

let merge (la, ra) (lb, rb) = (* Zawsze łączy dwa przedziały w jeden *)
	if (la = neg_infinity) && (lb = neg_infinity) then neg_infinity, max ra rb
	else min la lb, infinity

let rec superMerge a b = (* Scala wszystko ze wszystkim - odcinek z odcinkiem, odcinek z parą, parę z parą -> na koniec zawsze zwróci Odcinek, Parę albo Puste *)
	match a, b with
	| Odcinek (la, ra), Odcinek (lb, rb) ->
		if (ra >= lb) then Odcinek (la, rb)
		else if (rb >= la) then Odcinek (lb, ra) (* W razie gdyby [lb, rb] < [la, ra] *)
		else Para ((la, ra), (lb, rb))
	| Odcinek (l, r), Para ((l1, r1), (l2, r2)) ->
		if (l1 <= l && r <= r1) || (l2 <= l && r <= r2) then Para ((l1, r1), (l2, r2)) (* Pierwszy zawiera sie cały w lewym lub w prawym *)
		else if (l <= r1 && r >= l2) then Odcinek (min l l1, max r r2) (* Pierwszy zawiera dopełnienie drugiego przedziału *)
		else if (l <= r1 && r1 <= r && r < l2) then Para ((min l l1, r), (l2, r2)) (* Pierwszy zawiera cały lub część lewego przedziału *) 
		else if (r1 < l && l <= l2 && l2 <= r) then Para ((l1, r1), (l, max r r2)) (* Pierwszy zawiera cały lub część prawego przedziału *)
		else if (l1 <= r && r <= r1) then Para ((min l l1, r1), (l2, r2)) (* Pierwszy zawiera część lewego przedziału *)
		else Para ((l1, r1), (l2, max r2 r)) (* Pierwszy zawiera część drugiego przedziału *)
	| Para ((l1, r1), (l2, r2)), Odcinek (l, r) -> (* Tak samo jak we wcześniejszym przypadku *)
		if (l1 <= l && r <= r1) || (l2 <= l && r <= r2) then Para ((l1, r1), (l2, r2))
		else if (l <= r1 && r >= l2) then Odcinek (min l l1, max r r2)
		else if (l <= r1 && r1 <= r && r < l2) then Para ((min l l1, r), (l2, r2)) 
		else if (r1 < l && l <= l2 && l2 <= r) then Para ((l1, r1), (l, max r r2))
		else if (l1 <= r && r <= r1) then Para ((min l l1, r1), (l2, r2))
		else Para ((l1, r1), (l2, max r2 r))
	| Para ((l1, r1), (l2, r2)), Para ((l3, r3), (l4, r4)) -> (* Aby scalić parę z parą, mogę scalić dwukrotne scalenie pary z odcinkiem *)
		let pom = superMerge (Para ((l1, r1), (l2, r2))) (Odcinek (l3, r3)) in
		superMerge pom (Odcinek (l4, r4))
	| Odcinek (_, _), Puste -> Puste
	| Puste, Odcinek (_, _) -> Puste
	| Para ((_, _),(_, _)), Puste -> Puste
	| Puste, Para ((_, _),(_, _)) -> Puste
	| Puste, Puste -> Puste

let odwroc x = (* Przyjmuje x różny od 0, zwraca jego odwrotność *)
	if (x = neg_infinity || x = infinity) then 0.
	else 1. /. x

let odwrocPrzedzial (l, r) = (* Zamienia przedział na jego odwrotność *)
	if (l = neg_infinity && r = infinity) then Odcinek (l, r) (* [-inf, inf] -> [-inf, inf] *)
	else if (l = 0.) then begin
		if (r = 0.) then Puste (* [0, 0] -> nan *)
		else if (r = infinity) then Odcinek (l, r) (* [0, inf] -> [0, inf] *)
		else Odcinek (odwroc r, infinity) (* [0, x] -> [1/x, inf] *)
	end
	else if (r = 0.) then begin
		if (l = neg_infinity) then Odcinek (l, r) (* [-inf, 0] -> [-inf, 0] *)
		else Odcinek (neg_infinity, odwroc l) (* [x, 0] -> [-inf, 1/x] *)
	end
	else if (l < 0. && 0. < r) then (* Przedział [a, b], gdzie a <= 0 <= b *)
		Para ((neg_infinity, odwroc l), (odwroc r, infinity))
	else Odcinek (odwroc r, odwroc l) (* Każdy inny przypadek *)

let odwrocPare (la, ra) (lb, rb) = (* Zamienia parę przedziałów na ich odwrotności *)
	let odw1 = odwrocPrzedzial (la, ra) in
	let odw2 = odwrocPrzedzial (lb, rb) in
	superMerge odw1 odw2

(* KONSTRUKTORY *)

let wartosc_dokladnosc x p = (* [x - p%, x + p%] *)
	Odcinek ((x -. (p /. 100.) *. (abs x)), (x +. (p /. 100.) *. (abs x)))

let wartosc_od_do x y = (* [x, y] *)
	Odcinek (x, y)

let wartosc_dokladna x = (* [x, x] *)
	Odcinek (x, x)

(* SELEKTORY *)

let in_wartosc x y = (* Zwraca true gdy y należy do x *)
	match x with
	| Odcinek (l, r) ->
		not(czyNAN y) && l <= y && y <= r
	| Para ((l1, r1), (l2, r2)) -> 
		not(czyNAN y) && ((l1 <= y && y <= r1) || (l2 <= y && y <= r2))
	| Puste -> czyNAN y

let min_wartosc x = (* Zwraca minimalną wartość przedziału *)
	match x with
	| Odcinek (l, _) -> l
	| Para ((l, _), (_, _)) -> l
	| Puste -> nan

let max_wartosc x = (* Zwraca maksymalną wartość przedziału *)
	match x with
	| Odcinek (_, r) -> r
	| Para ((_, _), (_, r)) -> r
	| Puste -> nan

let sr_wartosc x = (* Zwraca średnią wartość przedziału, bądź nan *)
	(min_wartosc x +. max_wartosc x) /. 2.

(* MODYFIKATORY *)

let plus a b = (* Funkcja wykonująca operację dodawania przedziałów *)
	match a, b with 
	| Odcinek (la, ra), Odcinek (lb, rb) ->
		Odcinek (la +. lb, ra +. rb)
	| Odcinek (l, r), Para ((_, r1), (l2, _)) ->
		polacz (Para ((neg_infinity, r +. r1), (l +. l2, infinity)))
	| Para ((_, r1), (l2, _)), Odcinek (l, r) ->
		polacz (Para ((neg_infinity, r +. r1), (l +. l2, infinity)))
	| Para ((_, _), (_, _)), Para ((_, _), (_, _)) ->
		Odcinek (neg_infinity, infinity)
	| Odcinek (_, _), Puste -> Puste
	| Puste, Odcinek (_, _) -> Puste
	| Para ((_, _),(_, _)), Puste -> Puste
	| Puste, Para ((_, _),(_, _)) -> Puste
	| Puste, Puste -> Puste

let minus a b = (* Funkcja wykonująca operację odejmowania przedziałów *)
	match a, b with
	| Odcinek (la, ra), Odcinek (lb, rb) ->
		Odcinek (la -. rb, ra -. lb)
	| Odcinek (l, r), Para ((_, r1), (l2, _)) -> (* Dla tych dwóch przypadków wywołuje dodawanie wartości przeciwnych *)
		plus (Odcinek (l, r)) (Para ((neg_infinity, przeciwne l2), (przeciwne r1, infinity))) 
	| Para ((l1, r1), (l2, r2)), Odcinek (l, r) ->
		plus (Para ((l1, r1), (l2, r2))) (Odcinek (przeciwne r, przeciwne l))
	| Para ((_, _), (_, _)), Para ((_, _), (_, _)) ->
		Odcinek (neg_infinity, infinity)
	| Odcinek (_, _), Puste -> Puste
	| Puste, Odcinek (_, _) -> Puste
	| Para ((_, _),(_, _)), Puste -> Puste
	| Puste, Para ((_, _),(_, _)) -> Puste
	| Puste, Puste -> Puste

let razy a b = (* Funkcja wykonująca operację mnożenia przedziałów *)
	match a, b with
	| Odcinek (la, ra), Odcinek (lb, rb) ->
		Odcinek (pomnoz (la, ra) (lb, rb))
	| Odcinek (l, r), Para ((l1, r1), (l2, r2)) ->
		let p1 = pomnoz (l, r) (l1, r1) in
		let p2 = pomnoz (l, r) (l2, r2) in
		polacz (Para ((min p1 p2), (max p1 p2))) (* Próbuje połączyć pomnożone przedziały *)
	| Para ((l1, r1), (l2, r2)), Odcinek (l, r) ->
		let p1 = pomnoz (l, r) (l1, r1) in
		let p2 = pomnoz (l, r) (l2, r2) in
		polacz (Para ((min p1 p2), (max p1 p2))) (* Tak jak wcześniej *)
	| Para ((l1, r1), (l2, r2)), Para ((l3, r3), (l4, r4)) ->
		let p1 = pomnoz (l1, r1) (l3, r3) in
		let p2 = pomnoz (l1, r1) (l4, r4) in
		let p3 = pomnoz (l2, r2) (l3, r3) in
		let p4 = pomnoz (l2, r2) (l4, r4) in
		let wynik1 = merge (min p1 p4) (max p1 p4) in (* Łączy odpowiednie przedziały *)
		let wynik2 = merge (min p2 p3) (max p2 p3) in (* Tak samo *)
		polacz (Para ((min wynik1 wynik2), (max wynik1 wynik2))) (* Ostateczne łączenie *)
	| Odcinek (_, _), Puste -> Puste
	| Puste, Odcinek (_, _) -> Puste
	| Para ((_, _),(_, _)), Puste -> Puste
	| Puste, Para ((_, _),(_, _)) -> Puste
	| Puste, Puste -> Puste

let podzielic a b = (* Funkcja wykonująca operację dzielenia przedziałów *)
	match a, b with (* zamiast dzielić, funkcja ta wywołuje mnożenie przedziałów, gdzie drugi element mnożenia to przedziały odwrotne do pierwotnych *)
	| Odcinek (la, ra), Odcinek (lb, rb) ->
		let odw = odwrocPrzedzial (lb, rb) in
		razy (Odcinek (la, ra)) odw
	| Odcinek (l, r), Para ((l1, r1), (l2, r2)) ->
		let odw = odwrocPare (l1, r1) (l2, r2) in
		razy (Odcinek (l, r)) odw
	| Para ((l1, r1), (l2, r2)), Odcinek (l, r) ->
		let odw = odwrocPrzedzial (l, r) in
		razy (Para ((l1, r1), (l2, r2))) odw
	| Para ((l1, r1), (l2, r2)), Para ((l3, r3), (l4, r4)) ->
		let odw = odwrocPare (l3, r3) (l4, r4) in
		razy (Para ((l1, r1), (l2, r2))) odw
	| Odcinek (_, _), Puste -> Puste
	| Puste, Odcinek (_, _) -> Puste
	| Para ((_, _),(_, _)), Puste -> Puste
	| Puste, Para ((_, _),(_, _)) -> Puste
	| Puste, Puste -> Puste
