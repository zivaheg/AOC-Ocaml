let ime_datoteke = "input.txt"

let preberi_datoteko ime_datoteke =
    let chan = open_in ime_datoteke in
    let vsebina = really_input_string chan (in_channel_length chan) in
    close_in chan;
    vsebina

let podatki_sez = 
  (* output je str dolžine 17204 v obliki "R9\nL8\nL26\nR45\nR40\nL45..." *)
  let vsebina = preberi_datoteko ime_datoteke in
  let seznam = String.trim vsebina |> String.split_on_char '\n' in
  let obrnjen_sez = List.rev seznam in
  obrnjen_sez

let poberi_int str = String.sub str 1 (String.length str - 1) 
  |> int_of_string 

(*  (* wanna be ostanek pri deljenju s 100*) - nažalost št niso omejena na 0-99
let obrat pobrana_vsota =
  match pobrana_vsota with
  | x when x < 0 -> 100 + x
  | x when x > 99 -> x - 100
  | x -> x *)

let obrat preberana_vsota = 
  Int.rem preberana_vsota 100

let rec nal1 podatkov_seznam =
  match podatkov_seznam with
  | [] -> (50, 0)
  | x :: xs -> 
    let (old, counter) = nal1 xs in
    let new_val = 
      match x with
      | y when String.contains y 'R' -> obrat (old + poberi_int y)
      | y when String.contains y 'L' -> obrat (old - poberi_int y)
      | y -> failwith ("shouldn't happen, napačni str: " ^ y ) in
    let zeros =
        match new_val with
        | 0 -> counter + 1
        | _ -> counter
      in
    (new_val, zeros)

(* PRAVILNI ODGOVOR: int * int = (22, 1034) aka 1034*)

(*    ALT REŠITEV Z FOLD RIGHT

let prehod list =
  let step elem (value, zeros) =
    let new_val =
      match elem with
      | s when String.contains s 'R' ->
          obrat (value + poberi_int s)
      | s when String.contains s 'L' ->
          obrat (value - poberi_int s)
      | _ ->
          failwith "shouldn't happen"
    in
    let zeros' =
      match new_val with
      | 0 -> zeros + 1
      | _ -> zeros
    in
    (new_val, zeros')
  in
  List.fold_right step list (50, 0)
*)

(* -------------------------------------------NALOGA 2-------------------------------------------------*)

let obrat preberana_vsota = 
  let dosezeno = ((preberana_vsota mod 100) + 100) mod 100 in
  dosezeno

let abs x = 
  match x with 
  | y when y >= 0 -> y
  | y -> -y

let sign x =
  match x with 
  | 0 -> 0
  | y when y > 0 -> 1
  | _ -> -1


let floor_div a b =
  let q = a / b in
  match (a >= 0, a mod b = 0) with
  | (true, _) -> q
  | (false, true) -> q
  | (false, false) -> q - 1

let prehodi old korak =
  match korak with
  | 0 -> 0
  | k when k > 0 ->
      let start = old in
      let stop = old + k in
      let cnt = (floor_div stop 100) - (floor_div start 100) in
      if cnt >= 0 then cnt else 0
  | k ->

      let start = old in
      let stop = old + k in
      let cnt = (floor_div (start - 1) 100) - (floor_div (stop - 1) 100) in
      if cnt >= 0 then cnt else 0

let rec nal2 podatkov_seznam =
  match podatkov_seznam with
  | [] -> (50, 0)
  | x :: xs ->
      let (old, counter) = nal2 xs in

      let korak =
        match x with
        | y when String.contains y 'R' -> poberi_int y
        | y when String.contains y 'L' -> - (poberi_int y)
        | y -> failwith ("shouldn't happen, napačni str: " ^ y) in
      let vsota = old + korak in
      let new_val = obrat vsota in
      let wraps = prehodi old korak in
      let zeros = counter + wraps in

      (new_val, zeros)

(* PRAVILNI ODGOVOR: int * int = (22, 6166) aka 6166*)

(* let rec nal2 podatkov_seznam =
  match podatkov_seznam with
  | [] -> (50, 0)
  | x :: xs -> 
    let (old, counter) = nal2 xs in
    let korak = 
      match x with
      | y when String.contains y 'R' -> poberi_int y
      | y when String.contains y 'L' -> - (poberi_int y)
      | y -> failwith ("shouldn't happen, napačni str: " ^ y ) in
    let vsota = old + korak in
      let new_val = obrat vsota in
      let wraps = prehodi old korak in
      let zeros = counter + wraps in
  (new_val, zeros)*)

(* let rec nal2 podatkov_seznam =
match podatkov_seznam with
| [] -> (50,0)
| x :: xs ->
  let (old, counter) = nal2 xs in
  let korak =
    match x with
    | y when String.contains y 'R' -> poberi_int y
    | y when String.contains y 'L' -> - (poberi_int y)
    | y -> failwith ("shouldn't happen, napačni str: " ^ y ) in

    let vsota = old + korak in
    let new_val = obrat vsota in

    let pribitek = if (sign old = sign new_val && sign old <> 0) then 0
    else abs (vsota / 100) in
    
    let zeros =
    match new_val with
    | 0 -> counter + 1 + pribitek
    | _ -> counter + pribitek in

  (new_val, zeros)*)
