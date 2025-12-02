let ime_datoteke = "input-day2.txt"


(* output je str dolžine 17204 v obliki "4077-5314,527473787-527596071,709-872..." *)
let preberi_datoteko ime_datoteke =
    let chan = open_in ime_datoteke in
    let vsebina = really_input_string chan (in_channel_length chan) in
    close_in chan;
    vsebina

let range_str start fin =
  List.init (fin - start) (fun x -> string_of_int (start + x))

let range_sez sez =
  let start = List.hd sez |> int_of_string in
  let fin = List.rev sez |> List.hd |> int_of_string in
  range_str start fin

let podatki_sez_sez = 
  let vsebina = preberi_datoteko ime_datoteke in
  let seznam = String.trim vsebina |> String.split_on_char ',' in
  let sez_sez_range = List.map (String.split_on_char '-') seznam
  |> List.map range_sez in
  sez_sez_range
  
(* alternativna ideja je deljenje s 100 in mod 100, da ne skačes int -> str -> int*)

let accept str =
  let dolzina = String.length str in
  let dolzina_pol = dolzina / 2 in (*pri lihih je celi del*)
  let prvi = String.sub str 0 dolzina_pol in 
  let drugi = String.sub str dolzina_pol  (dolzina - dolzina_pol) in (* pri lihih dosežejo različno dolžino*)
  String.equal prvi drugi 

let rec sum_sez sez =
  match sez with
  | [] -> 0
  | x :: xs -> sum_sez xs + x

  let nal1 = 
    List.map (List.filter accept) podatki_sez_sez
    |> List.flatten
    |> List.map int_of_string
    |> sum_sez 

(*PRAVILNI ODGOVOR: int = 13108371860*)

(*-----------------------------------NALOGA 2-----------------------------------------------*)

let is_periodic str =
  let dolzina = String.length str in
  let dolzina_pol = dolzina / 2 in

  let rec try_i i =
  if i > dolzina_pol then false
  else
    match dolzina mod i with
    | r when r <> 0 -> try_i (i + 1)

    | _ ->
      let base = String.sub str 0 i in

      let rec check j =
        match j >= dolzina with
        | true -> true
        | false ->
          match j + i > dolzina with
          | true -> false
          | false ->
            let seg = String.sub str j i in
            match seg = base with
            | true -> check (j + i)
            | false -> false in

      match check i with
      | true -> true
      | false -> try_i (i + 1) in
  try_i 1


let nal2 =
  podatki_sez_sez
  |> List.map (List.filter is_periodic)
  |> List.flatten
  |> List.map int_of_string
  |> sum_sez

(* PRAVILNI ODGOVOR: int = 22471660255*)
