
module Pair = struct 

  type t = [ `XZECZEUR | `ADAEUR | `XTZEUR ]
  [@@deriving show]

  let of_string = function
    | "XZECZEUR" -> `XZECZEUR
    | "ADAEUR" -> `ADAEUR
    | "XTZEUR" -> `XTZEUR
    | s -> failwith @@ "Error parsing pair: '"^s^"'"
  
end


module Typ = struct 

  type t = [ `Buy | `Sell ]
  [@@deriving show]

  let of_string = function
    | "buy" -> `Buy
    | "sell" -> `Sell
    | s -> failwith @@ "Error parsing typ: '"^s^"'"

end

(*example value: 2020-03-14 18:06:46.2255*)
let parse_time str =
  let split = CCString.split_on_char in
  let int = CCInt.of_string_exn in
  let [ date_str; time_str ] = split ' ' str in
  let [ y; m; d ] = split '-' date_str in
  let date = int y, int m, int d in
  let [ h; m; s ] = split ':' time_str in
  let [ s; _ ] = split '.' s in
  let tz = 0 in 
  let time = int h, int m, int s in
  Ptime.of_date_time (date, (time, tz))
  |> CCOption.get_exn_or "Ptime error"

module Entry = struct 

  module T = struct 
  
    type t = {
      time : Ptime.t;
      pair : Pair.t;
      typ : Typ.t;
      cost : float; (*eur*)
      vol : float; (*crypto*)
    }[@@deriving show]

  end
  include T

  let of_csv_row row =
    let r k = Csv.Row.find row k in
    let time = parse_time @@ r "time" in
    let pair = Pair.of_string @@ r "pair" in
    let typ = Typ.of_string @@ r "type" in
    let cost = Float.of_string @@ r "cost" in
    let vol = Float.of_string @@ r "vol" in
    { time; pair; typ; cost; vol }
  
end

open Entry.T

(*goto compare time with Ptime.is_earlier*)

let main () = 
  match Sys.argv |> Array.to_list |> CCList.drop 1 with
  | csv_path :: year :: pair :: [] ->
    let year = match year with
      | "all" -> None
      | year -> Some (CCInt.of_string_exn year)
    in
    let pair = match pair with
      | "all" -> None
      | pair -> Some (Pair.of_string pair)
    in
    let entries = 
      csv_path
      |> Csv.Rows.load ~has_header:true 
      |> List.map Entry.of_csv_row
      |> List.filter (fun e ->
        let (e_y, _, _) = Ptime.to_date e.time in
        let is_year = year |> CCOption.for_all (CCInt.equal e_y) in
        let is_pair = pair |> CCOption.for_all ((=) e.pair) in
        is_year && is_pair
      )
    in
    entries
    |> CCList.to_string Entry.show
    |> print_endline
  | _ ->
    Printf.eprintf "Usage: %s <csv> <year|'all'> <pair|'all'>" (Sys.argv.(0));
    exit 1

let () = main ()
