
module Pair = struct 

  module T = struct 
  
    type t = [ `XZECZEUR | `ADAEUR | `XTZEUR ]
    [@@deriving show,eq,ord]

  end
  include T

  let of_string = function
    | "XZECZEUR" -> `XZECZEUR
    | "ADAEUR" -> `ADAEUR
    | "XTZEUR" -> `XTZEUR
    | s -> failwith @@ "Error parsing pair: '"^s^"'"

  module Map = CCMap.Make(T)
  
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
      price : float; (*eur*)
      vol : float; (*crypto*)
    }[@@deriving show]

  end
  include T

  let of_csv_row row =
    let r k = Csv.Row.find row k in
    let time = parse_time @@ r "time" in
    let pair = Pair.of_string @@ r "pair" in
    let typ = Typ.of_string @@ r "type" in
    let price = Float.of_string @@ r "price" in
    let vol = Float.of_string @@ r "vol" in
    { time; pair; typ; price; vol }
  
end

open Entry.T

module Stats = struct 

  type yearly_result = {
    wins : float;
    losses : float;
  }[@@deriving show]
  
  (*goto acc wins/losses per year*)
  type t = {
    pair : Pair.t;
    yearly_results : (int * yearly_result) list;
    buys_left : Entry.t list;
  }[@@deriving show]

  let calc ~pair ~buys ~sells =
    let rec aux acc sell =
      match acc.buys_left with
      | [] -> failwith "No buys left.. something is wrong"
      | buy :: buys_left ->
        assert (Ptime.is_later sell.time ~than:buy.time);
        begin match acc.yearly_results with
          | [] ->
            let year, _, _ = Ptime.to_date sell.time in
            (*goto howto
              * try to consume buy.vol with sell.vol
                * if sell.vol < buy.vol then
                  * map buy and append to buys_left
                  * calc win/loss based on buy.price, sell.price and sell.vol
                    * if sold at a loss, return the loss
                    * else return the win
                * if sell.vol >= buy.vol then
                  * calc win/loss based on buy.price, sell.price and sell.vol
                    * if sold at a loss, add loss to yearly result
                    * else add to yearly win
                    * => add to acc: yearly_results + buys_left
                    * => recursively call aux with acc and { sell with vol - buy.vol }
                  
            *)
            let wins, losses, buys_left =
              failwith "todo"
            in
            let res = { wins; losses } in
            let yearly_results = (year, res) :: [] in
            { acc with yearly_results; buys_left }
          | (year, yres) :: yres_rest ->
            failwith "todo"
        end
    in
    let buys_left =
      buys |> CCList.sort (fun e e' -> Ptime.compare e.time e'.time)
    in
    let init = { pair; yearly_results = []; buys_left } in
    sells
    |> CCList.sort (fun e e' -> Ptime.compare e.time e'.time)
    |> List.fold_left aux init

end

let main () = 
  match Sys.argv |> Array.to_list |> CCList.drop 1 with
  | csv_path :: [] ->
    let entries = 
      csv_path
      |> Csv.Rows.load ~has_header:true 
      |> List.map Entry.of_csv_row
    in
    let entries_per_pair =
      entries
      |> List.fold_left (fun acc e ->
        acc |> Pair.Map.update e.pair (function
          | None -> Some [e]
          | Some es -> Some (e::es)
        )
      ) Pair.Map.empty
      |> Pair.Map.map List.rev
    in
    entries_per_pair |> Pair.Map.iter (fun pair entries ->
      (* Format.printf "----- Entries for %a\n%!" Pair.pp pair; *)
      (* entries *)
      (* |> CCList.to_string Entry.show *)
      (* |> print_endline *)
      let buys, sells = entries |> CCList.partition (fun e -> e.typ = `Buy) in
      let stats = Stats.calc ~pair ~buys ~sells in
      Format.printf "%a" Stats.pp stats
    )
  | _ ->
    Printf.eprintf "Usage: %s <csv> <year|'all'> <pair|'all'>" (Sys.argv.(0));
    exit 1

let () = main ()
