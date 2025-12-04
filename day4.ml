let ime_datoteke = "input-day4.txt"

(* output je str dolžine 17204 v obliki "@@@@..@ |...| @...@.@\n.@@" *)
let preberi_datoteko ime_datoteke =
    let chan = open_in ime_datoteke in
    let vsebina = really_input_string chan (in_channel_length chan) in
    close_in chan;
    vsebina

(* v Pythonu to nrdiš z convolution, tle pa pač na dolgo*)

    (* day 3 - altered *)
let rec list_of_str str =
  match str with
  | "" -> []
  | _ ->
      let first = String.sub str 0 1 in
      let rest_len = String.length str - 1 in
      let rest =
        if rest_len > 0 then String.sub str 1 rest_len else "" in
      first :: list_of_str rest

let podatki_sez =
  let podani = 
  preberi_datoteko ime_datoteke
  |> String.trim
  |> String.split_on_char '\n' 
  |> List.map (fun x -> "." ^ x ^ ".") in
  let dol = String.length (List.hd podani) in
  let prazna = String.make dol '.' in
  [prazna] @ podani @ [prazna]
  |> List.map list_of_str


(*Source - https://stackoverflow.com/a
  Posted by ivg
  Retrieved 2025-12-04, License - CC BY-SA 4.0*)

let array_to_list xs =
  Array.fold_right List.cons xs []

let array_of_list xs = match xs with
  | [] -> [||]
  | default :: _ ->
    let arr = Array.make (List.length xs) default in
    List.iteri (Array.set arr) xs;
    arr
(* end *)

let matrix = 
  List.map array_of_list podatki_sez 
  |> array_of_list

let prestej_kvadrat i j mat = 
  let preveri = [(i - 1, j - 1); (i - 1, j); (i - 1, j + 1);  (* lahko bi s for loop napisala*)
                   (i, j - 1);                  (i, j + 1);
                 (i + 1, j - 1); (i + 1, j); (i + 1, j + 1)] in
  let rec aux sez =
    match sez with
    | [] -> 0
    | (ii, jj) :: xs -> 
      if (mat.(ii).(jj) = "@" || mat.(ii).(jj) = "x" ) then 1 + aux xs
      else 0 + aux xs in
  aux preveri

let kvadratek i j mat =
  if (prestej_kvadrat i j mat) < 4 && mat.(i).(j) = "@" then mat.(i).(j) <- "x"
  
let dopisani_x = (* spremeni se matrix, ta je unit*)
  let matrika = Array.copy matrix in (* it'll be a good day k bo copy dejansko delov :( *)
  let len = Array.length matrika.(0) in
  for i = 1 to (len - 2) do
    for j = 1 to (len - 2) do
      kvadratek i j matrika
    done
  done
  
let prestej_x str =
  if str = "x" then 1
  else 0

(* day2 *)
let rec sum_sez sez =
  match sez with
  | [] -> 0
  | x :: xs -> sum_sez xs + x

let nal1 =
  Array.map array_to_list matrix
  |> array_to_list
  |> List.flatten
  |> List.map prestej_x
  |> sum_sez

(*PRAVILNI ODGOVOR: int = 1435*)

(*-----------------------------------NALOGA 2-----------------------------------------------*)

let str_to_01 x =
  match x with
    | "@" -> 1
    | "x" -> 1  
    | _ -> 0

let matrix_to_int mat =
  Array.map (Array.map str_to_01)  mat

let stevec_sosedov_int i j mat =
  let preveri = [(i - 1, j - 1); (i - 1, j); (i - 1, j + 1);  (* lahko bi s for loop napisala*)
                   (i, j - 1);                  (i, j + 1);
                 (i + 1, j - 1); (i + 1, j); (i + 1, j + 1)] in
  let rec aux sez =
    match sez with
    | [] -> 0
    | (ii,jj)::xs ->
        if mat.(ii).(jj) = 1 then 1 + aux xs
        else aux xs in
  aux preveri

let is_accessible mat =
  let height = Array.length mat in
  let width = Array.length mat.(0) in
  let dostopni = Array.make_matrix height width 0 in

  for i = 1 to height - 2 do
    for j = 1 to width - 2 do
      match mat.(i).(j) with
      | 0 -> ()   
      | _ ->
          let st = stevec_sosedov_int i j mat in
          if st < 4 then dostopni.(i).(j) <- 1
    done
  done; (* in po for loop napišemo kot ; *)

  let total = (* array_to_list sum_sez *)
    Array.fold_left (fun acc row -> acc + Array.fold_left (+) 0 row) 0 dostopni in
  (total, dostopni)

let apply_acces mat dostopni =
  let h = Array.length mat in
  let w = Array.length mat.(0) in
  for i = 0 to h - 1 do
    for j = 0 to w - 1 do
      mat.(i).(j) <- max 0 (mat.(i).(j) - dostopni.(i).(j))
    done
  done

let nal2 =
  let mat = matrix_to_int matrix in
  let rec loop acc =
    let (yesno, dostopni) = is_accessible mat in
    match yesno with
    | 0 -> acc
    | n ->
      let _ = apply_acces mat dostopni in
      loop (acc + n) in

  loop 0

(*PRAVILNI ODGOVOR: int = 8623 *)



