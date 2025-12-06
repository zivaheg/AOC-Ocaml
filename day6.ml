let ime_datoteke = "input-day6.txt"

(* output je str dolžine 17204 v obliki "91  99 31  33 125 61   44 4121..." *)
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
(* List.map String.length podatki_sez;;
- : int list = [3720; 3720; 3720; 3720; 3719]*)
let dodaj_space sez =
  let preveri_kateri str = if String.length str = 3719 then (str ^ " ") else str in
  List.map preveri_kateri sez
  
let podatki_sez = dodaj_space podatki_sez

let podatki_sez_sez =
    List.map (String.split_on_char ' ') podatki_sez
    |> List.map (List.filter (fun x -> if x = "" then false else true))

(* day 2 - altered *)
let rec sum_sez sez =
  match sez with
  | [x] when String.length x = 1 -> 0
  | x :: xs -> sum_sez xs + (int_of_string x)
  | _ -> failwith "shouldn't happen"

let rec prod_sez sez =
  match sez with
  | [x] when String.length x = 1 -> 1
  | x :: xs -> prod_sez xs * (int_of_string x)
  | _ -> failwith "shouldn't happen"

let rec po_sez sez_sez vsota = 
  let len = List.length sez_sez in
  match  List.nth sez_sez (len - 1) with
  | [] -> vsota
  | "+" :: _ -> 
    let xs = List.map List.tl sez_sez in
    let sum = List.map (List.hd) sez_sez |> sum_sez in
    po_sez xs (sum + vsota)
  | "*" :: _ -> 
    let xs = List.map List.tl sez_sez in
    let sum = List.map (List.hd) sez_sez |> prod_sez in
    po_sez xs (sum + vsota)
  | _ -> failwith "shouldn't happen"

  let nal1 = po_sez podatki_sez_sez 0

  (*PRAVILNI ODGOVOR: int = 4771265398012 *)

(*-----------------------------------NALOGA 2-----------------------------------------------*)

let charList_of_str str =
  List.init (String.length str) (String.get str)

let ch_sez_sez str =
  List.map charList_of_str str

let space_column matrix j =
  List.for_all (fun row -> row.(j) = ' ') matrix

let to_matrix matrix =
  List.map Array.of_list matrix

let split_space row split_indices =
  let rec aux start splits =
    match splits with
    | [] -> [Array.sub row start (Array.length row - start)]
    | idx :: rest ->
        let chunk = Array.sub row start (idx - start) in
        chunk :: aux (idx + 1) rest in
  aux 0 split_indices

let split_matrix str =
  let matrix = ch_sez_sez str |> to_matrix in
  let len = Array.length (List.hd matrix) in
  let split_points =
    List.filter (fun j -> space_column matrix j) (List.init len Fun.id) in
  List.map (fun row -> split_space row split_points) matrix

  (* char array list list = [[[|'9'; '1'; ' '|]; [|'9'; '9'|]; [|'3'; '1'|]; ... ] ... ] 
   char array array array = [| [| [|'9'; '1'; ' '|]; [|'9'; '9'|]; [|'3'; '1'|]; ... |] ... |] *)
  let podatki_mat = 
    split_matrix podatki_sez
    |> to_matrix
    |> Array.of_list

let string_of_chArray array =
  String.init (Array.length array) (fun i -> array.(i))

let poberi_info matrix group_i = 
  let n_rows = Array.length matrix in
  let cols = Array.length matrix.(0).(group_i) in
      let oper_string =
        let first_char = matrix.(n_rows - 1).(group_i).(0) in
        String.make 1 first_char |> String.trim in
      let normal_strings = Array.init cols ( fun column_i ->
          let column_chars =
            Array.init (n_rows - 1) (fun ele -> matrix.(ele).(group_i).(column_i)) in
          string_of_chArray column_chars |> String.trim) in
      Array.append [| oper_string |] normal_strings

let extract_columns matrix =
  let n_groups = Array.length matrix.(0) in
  let column_indices = Array.init n_groups (fun i -> n_groups - 1 - i) in
  Array.map (poberi_info matrix) column_indices

let podatki = extract_columns podatki_mat

let evaluate_row row =
  let op = row.(0) in
  let nums =
    Array.to_list (Array.sub row 1 (Array.length row - 1))
    |> List.map (fun str -> int_of_string (String.trim str)) in
  match op with
  | "+" -> List.fold_left ( + ) 0 nums
  | "*" -> List.fold_left ( * ) 1 nums
  | _ -> failwith "shouldn't happen"

let evaluate_matrix mat =
  Array.to_list mat
  |> List.map evaluate_row
  |> List.fold_left ( + ) 0

let nal2 = evaluate_matrix podatki

 (*PRAVILNI ODGOVOR: int = 10695785245101 *)