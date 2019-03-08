(*
    Zadanie: Przelewanka
    Autor: Mateusz Danowski
    Code Review: Marcin Abramowicz
*)

let rec nwd a b = (* Funkcja oblicza największy wspólny dzielnik [a] i [b] *)
    if b = 0 then a else nwd b (a mod b)

let zalozenia a = (* Funkcja sprawdza, czy da się uzyskać oczekiwany wynik *)
    let pojemnosc = List.exists (fun (x, y) -> x = y || y = 0) (Array.to_list a)
    and nwd_x = Array.fold_left (fun acc (x, _) -> nwd acc x) (fst a.(0)) a
    and nwd_y = Array.fold_left (fun acc (_, y) -> nwd acc y) (snd a.(0)) a in
    let dzielenie = (nwd_y mod nwd_x = 0) in
    pojemnosc && dzielenie

let przelewanka szklanki =
    let szklanki = Array.of_list (List.filter (fun (x, _) -> x > 0) (Array.to_list szklanki)) in (* Ta sama tablica bez elementów [(0, 0)] *)
    let n = Array.length szklanki in
    if n = 0 then 0
    else if not (zalozenia szklanki) then -1 (* Jeśli nie da się uzyskać oczekiwanego wyniku, to wynikiem jest -1 *)
    else begin
        let stany = Hashtbl.create 1000000 (* Mapa do spamiętywania odwiedzonych już stanów *)
        and k = Queue.create ()
        and poczatkowy = Array.make n 0 (* Stan początkowy - wszystkie szklanki są puste *)
        and koncowy = Array.map (fun (_, y) -> y) szklanki in (* Stan końcowy - szklanki mają oczekiwaną ilość wody *)
        let dodaj stan kroki = (* Funkcja dodaje nowy stan na koniec kolejki *)
            if not (Hashtbl.mem stany stan) then
                Queue.add (stan, kroki) k in
        let nalej stan kroki =
            for i = 0 to n - 1 do (* Funkcja nalewa do każdej szklanki do pełna *)
                if stan.(i) < fst szklanki.(i) then begin
                    let nast_stan = Array.copy stan in
                    nast_stan.(i) <- fst szklanki.(i);
                    dodaj nast_stan (kroki + 1)
                end
            done in
        let wylej stan kroki = (* Funkcja wylewa całą wodę z każdej szklanki *)
            for i = 0 to n - 1 do
                if stan.(i) > 0 then begin
                    let nast_stan = Array.copy stan in
                    nast_stan.(i) <- 0;
                    dodaj nast_stan (kroki + 1)
                end
            done in
        let przelej stan kroki = (* Funkcja przelewa możliwie największą ilość wody pomiędzy każdą parą szklanek *)
            for i = 0 to n - 1 do
                for j = 0 to n - 1 do
                    if i <> j then begin
                        let przelej = min stan.(i) ((fst szklanki.(j)) - stan.(j)) in
                        if przelej > 0 then begin
                            let nast_stan = Array.copy stan in
                            nast_stan.(j) <- stan.(j) + przelej;
                            nast_stan.(i) <- stan.(i) - przelej;
                            dodaj nast_stan (kroki + 1)
                        end
                    end
                done
            done in
        Queue.add (poczatkowy, 0) k;
        let break = ref false in
        while not !break do (* Dopóki nie znalazłem stanu oczekiwanego *)
            let (stan, kroki) = Queue.pop k in
            if not (Hashtbl.mem stany stan) then begin
                Hashtbl.add stany stan kroki;
                if stan = koncowy then break := true
                else begin (* Jeśli obecny stan nie jest oczekiwanym, to wykonaj na nim wszystkie dostępne operacje *)
                    nalej stan kroki;
                    wylej stan kroki;
                    przelej stan kroki
                end
            end
        done;
        Hashtbl.find stany koncowy (* Minimalna liczba czynności potrzebnych do uzyskania oczekiwanego wyniku *)
    end
