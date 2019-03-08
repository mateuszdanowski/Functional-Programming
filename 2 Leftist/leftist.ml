(* 
	Zadanie: Drzewa Lewicowe
	Autor: Mateusz Danowski
	Code Review: Rafał Czarnecki
*)

(* lewy syn, element w wierzchołku, prawy syn, odległość wierzchołka od najbliższego liścia *)
type 'a node = {left: 'a queue; value: 'a; right: 'a queue; distance: int}
and 'a queue = Node of 'a node | Null

exception Empty

let make_node l x r d = (* tworzy wierzchołek *)
	Node ({left = l; value = x; right = r; distance = d})

let is_empty q =
	q = Null

let empty =
	Null

let rec join q1 q2 =
	match q1, q2 with
	| Null, _ -> q2
	| _, Null -> q1
	| Node (x), Node (y) ->
		if x.value > y.value then join (Node (y)) (Node (x)) (* zamiana wartosci x i y, aby rozważać jedynie przypadek x.value <= y.value *)
		else
			let Node (merged_tree) = join x.right (Node (y)) in (* wywołanie rekurencyjne w prawym poddrzewie *)
			if x.left = Null then make_node (Node (merged_tree)) x.value Null 0 (* nie ma lewego syna, więc prawy syn staje się lewym *)
			else
				let Node (l) = x.left in (* wybiera odpowiednie poddrzewo do bycia lewym/prawym synem *)
				if l.distance < merged_tree.distance then make_node (Node (merged_tree)) x.value x.left (l.distance + 1)
				else make_node x.left x.value (Node (merged_tree)) (merged_tree.distance + 1)

let add x q = (* tworzy drzewo z jednym wierzchołkiem x i łączy z drzewem q *)
	join (make_node Null x Null 0) q

let delete_min q =
	match q with
	| Null -> raise Empty
	| Node (x) -> (x.value, join x.left x.right)
