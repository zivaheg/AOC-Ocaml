let ime_datoteke = "input-day3.txt"

(* output je str dolžine 17204 v obliki "22154...3222423\n1222" *)
let preberi_datoteko ime_datoteke =
    let chan = open_in ime_datoteke in
    let vsebina = really_input_string chan (in_channel_length chan) in
    close_in chan;
    vsebina

let rec intList_of_str str =
  match str with
  | "" -> []
  | _ ->
      let first = int_of_string (String.sub str 0 1)  in
      let rest_len = String.length str - 1 in
      let rest =
        if rest_len > 0 then String.sub str 1 rest_len else "" in
      first :: intList_of_str rest

let podatki_sez =
  preberi_datoteko ime_datoteke
  |> String.trim
  |> String.split_on_char '\n' 
  |> List.map intList_of_str

let max_index sez =
  let rec aux i current_max current_idx s = 
    match s with
    | [] -> (current_max, current_idx)
    | x :: xs ->
      let last_id = List.length sez - 1 in
      if (x > current_max && i < last_id ) then aux (i + 1) x i xs
      else aux (i + 1) current_max current_idx xs in
  match sez with
  | [] -> failwith "Empty list"
  | x :: xs -> aux 1 x 0 xs

(* dn1 *)
let rec drop n list =
  match (n, list) with
  | (0, list) -> list
  | (_, []) -> []
  | (n, _ :: xs) -> drop (n - 1) xs

let find_pair sez =
  let (max1, id1) = max_index sez in
  let rest = drop (id1 + 1) sez in
  let (max2, _) = max_index (rest @ [0]) in
  max1 * 10 + max2

(* day2 *)
let rec sum_sez sez =
  match sez with
  | [] -> 0
  | x :: xs -> sum_sez xs + x

let nal1 =
  List.map find_pair podatki_sez
  |> sum_sez

(*PRAVILNI ODGOVOR: int = 17346*)

(*-----------------------------------NALOGA 2-----------------------------------------------*)


let max_index_upto sez ignore_last =
  let n = List.length sez in
  let limit = n - ignore_last in

  let rec aux i current_max current_idx s =
    match s with
    | [] -> (current_max, current_idx)
    | x :: xs ->
        if i >= limit then (current_max, current_idx)
        else
          match x > current_max with
          | true -> aux (i + 1) x i xs
          | false -> aux (i + 1) current_max current_idx xs in

  match sez with
  | [] -> failwith "Empty list"
  | x :: xs -> aux 1 x 0 xs

let find_k_max sez k =
  let rec aux list k ignore_last =
    match k with
    | 0 -> []
    | _ ->
        let (m, idx) = max_index_upto list ignore_last in
        let rest = drop (idx + 1) list in
        m :: aux rest (k - 1) (ignore_last - 1) in
  aux sez k 11

let rec number_of_digits sez =
  match sez with
  | [] -> 0
  | x :: xs -> x + 10 * number_of_digits xs

let nal2 =
  podatki_sez
  |> List.map (fun sez -> let digits = find_k_max sez 12 in 
                                    number_of_digits (List.rev digits))
  |> sum_sez

(*PRAVILNI ODGOVOR: int = 172981362045136*)