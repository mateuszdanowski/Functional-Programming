(*
    Zadanie: Sortowanie topologiczne
    Autor: Mateusz Danowski
    Code Review: Marcin Abramowicz
*)

open PMap;;

type state = Unvisited | Visited | Finished (* Status wierzchołka: nieodwiedzony, w trakcie przetwarzania, przetwarzanie zakończone *)
type 'a node = {mutable list: 'a list; mutable status: state} (* Typ mapy: lista sąsiadów, status wierzchołka *)

exception Cykliczne

let make_graph l =
    let rec do_nodes l map = (* Funkcja przechodzi po wszystkich wierzchołkach pojawiających się na listach sąsiedztwa i tworzy dla nich element w mapie *)
        match l with
        | [] -> map
        | (node, lst)::t ->
            let rec loop l acc =
                match l with
                | [] -> acc
                | h::t -> loop t (add h {list = []; status = Unvisited} acc) in
            do_nodes t (loop lst map) in

    let graph = do_nodes l empty in (* Reprezentacja grafu, jeszcze bez krawędzi i wierzchołków, które nie mają krawędzi wchodzących *)

    let rec do_edges l map = (* Funkcja przechodzi po wierzchołkach, które mają krawędzie wychodzące i tworzy dla nich listy sąsiedztwa *)
        match l with
        | [] -> map
        | (node, lst)::t ->
            if mem node map then (* Przypadek, gdy wierzchołek nie ma krawędzi wchodzących i pojawia się na wejściowej liście więcej niż raz *)
                let node_map = find node map in
                node_map.list <- lst @ node_map.list;
                do_edges t map
            else 
                do_edges t (add node {list = lst; status = Unvisited} map) in
    do_edges l graph

let topol l =
    let graph = make_graph l in (* Mapa reprezentująca graf *)
    let result = ref [] in (* Lista, na której będzie kumulowany wynik *)
    let rec dfs v = (* Funkcja dfs, przechodząca graf w głab *)
        let node = find v graph in
        match node.status with
        | Unvisited ->
            node.status <- Visited;
            List.iter dfs node.list; (* Wywołanie funkcji dfs na sąsiadach [v] *)
            result := v::(!result);
            node.status <- Finished
        | Visited -> raise Cykliczne
        | Finished -> () in
    List.iter (fun (v, _) -> dfs v) l; (* Wywołanie funkcji dfs na wierzchołkach, które mają krawędzie wychodzące *)
    !result
