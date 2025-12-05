let ime_datoteke = "input-day5.txt"

(* output je str dolžine 17204 v obliki "291687894568177-292172488078380\n4278..." *)
let preberi_datoteko ime_datoteke =
    let chan = open_in ime_datoteke in
    let vsebina = really_input_string chan (in_channel_length chan) in
    close_in chan;
    vsebina

let podatki_sez =
  let podani = 
  preberi_datoteko ime_datoteke
  |> String.trim
  |> String.split_on_char '\n' in
  podani

let vsebuje_r =
  String.exists (fun x -> x = '-')

let razdeli sez =
  List.partition vsebuje_r sez

let (id_range, available) = razdeli podatki_sez
let available = List.tl available 
  |> List.map int_of_string

(* day 2 - semi abandoned idea*)
let range_str start fin =
  List.init (fin - start) (fun x -> (start + x))

let range_sez sez =
  let start = List.hd sez in
  let fin = List.rev sez |> List.hd  in
  range_str start fin
(* end *)

let podatki_sez_sez = List.map (String.split_on_char '-') id_range
  |> List.map (List.map int_of_string)

let nahaja int par =
  match par with
  | [] -> false
  | [start; fin] -> 
    if (start <= int && int <= fin) then true else false
  | _ -> false

let poisci_v_range int =
  List.exists (nahaja int) podatki_sez_sez

  (* dn2 *)
let rec unique sez =
  match sez with
  | [] -> []
  | x :: xs -> 
    if List.mem x xs then unique xs
    else x :: unique xs
let preveri =
  List.filter poisci_v_range available
  |> unique

let nal1 = 
  List.length preveri

(*PRAVILNI ODGOVOR: int = 720*)

(*-----------------------------------NALOGA 2-----------------------------------------------*)

(* no need to count this one, sm preveč rabla gpt da sm sploh čez pršla :( *)


let v_tuple sez =
  match sez with
  | [s; e] -> (s, e)
  | _ -> failwith "shouldn't happen"

let intervals =
  List.map v_tuple podatki_sez_sez

(* events = [(137575293169902, 1); (139496917155869, -1); (412862672565369, 1); ...] *)
let events =          (* keepamo track of intervali, če je coverage > 0 smo na intervalu*)
  let rec aux acc list =
    match list with
    | [] -> acc
    | (s, e) :: xs ->
        aux ((s, 1) :: (e + 1, -1) :: acc) xs
  in
  aux [] intervals

(* [(424358604270, 1); (4902201785270, 1); (7668845009585, -1); ... ] *)
  let sorted_events =
  List.sort (fun (a, _) (b, _) -> compare a b) events

let nal2 =
  let rec sweep previous_postion current_coverage acc list =
    match list with
    | [] -> acc
    | (position, delta) :: xs ->
        let acc =
          match current_coverage > 0 with
          | true -> acc + (position - previous_postion)  (* št podvojenih števil*)
          | false -> acc
        in
        sweep position (current_coverage + delta) acc xs  (* nazaj odpremo ali zapremo interval *)
  in
  match sorted_events with
  | [] -> 0
  | (start_position, delta) :: rest ->
      sweep start_position delta 0 rest

(*PRAVILNI ODGOVOR: int = 357608232770687 *)


