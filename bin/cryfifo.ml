
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
      fee : float; (*eur*)
      margin : float; (*eur*)
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
    let fee = Float.of_string @@ r "fee" in
    let margin = Float.of_string @@ r "margin" in
    let vol = Float.of_string @@ r "vol" in
    { time; pair; typ; price; fee; margin; vol }
  
end

open Entry.T

module IntMap = struct
  include CCMap.Make(CCInt)
  let pp pp_v = pp CCInt.pp pp_v
end

let year_of_time t =
  let y, _, _ = Ptime.to_date t in
  y

module Stats = struct 

  type yearly_result = {
    wins : float;
    losses : float;
  }[@@deriving show]
  
  type t = {
    pair : Pair.t;
    yearly_results : yearly_result IntMap.t;
    buys_left : Entry.t list;
  }[@@deriving show]

  let update_yearly_results ~year ~wins ~losses v =
    v |> IntMap.update year (function
      | None -> Some { wins; losses }
      | Some yres -> 
        let wins, losses = yres.wins +. wins, yres.losses in
        Some { wins; losses }
    )

  let calc ~pair ~buys ~sells =
    let rec aux acc sell =
      match acc.buys_left with
      | [] ->
        (*Note: this can happen if staking gave extra volume*)
        Format.eprintf "WARNING: There is no buys left for %a - \
                        selling pot. staked volume %.2f eur\n%!"
          Pair.pp pair
          (sell.vol *. sell.price);
        let year = year_of_time sell.time in
        let wins, losses = sell.vol *. sell.price, 0. in
        let yearly_results =
          acc.yearly_results
          |> update_yearly_results ~year ~wins ~losses
        in
        { acc with yearly_results }
      | buy :: buys_left ->
        (*goto problem with staking:
          * if selling more than buying, because of gains of staking
            * then sells can 'overtake' buys
              * which leads to this assertion to fail
        *)
        (* assert (Ptime.is_later sell.time ~than:buy.time); *)
        if Ptime.is_earlier sell.time ~than:buy.time then (
          Format.eprintf "WARNING: sell time (%a) is earlier than buy time (%a) \
                          registering sell as win %a\n%!"
            Ptime.pp sell.time 
            Ptime.pp buy.time 
            Entry.pp buy;
          let year = year_of_time sell.time in
          let wins, losses = sell.vol *. sell.price, 0. in
          let yearly_results =
            acc.yearly_results
            |> update_yearly_results ~year ~wins ~losses
          in
          { acc with yearly_results }
        ) else 
          let year = year_of_time sell.time in
          if sell.vol < buy.vol then
            let buy = { buy with vol = buy.vol -. sell.vol } in
            let buys_left = buy :: buys_left in
            let wins, losses =
              let v = 
                sell.vol *. sell.price 
                -. (sell.vol *. buy.price +. buy.fee) 
              in
              if v >= 0. then v, 0. else 0., -.v 
            in
            let yearly_results =
              acc.yearly_results
              |> update_yearly_results ~year ~wins ~losses
            in
            { acc with yearly_results; buys_left }
          else (* sell.vol >= buy.vol *)
            let wins, losses =
              let v =
                buy.vol *. sell.price
                -. (buy.vol *. buy.price +. buy.fee)
              in
              if v >= 0. then v, 0. else 0., -.v 
            in
            let yearly_results =
              acc.yearly_results
              |> update_yearly_results ~year ~wins ~losses
            in
            let acc = { acc with yearly_results; buys_left } in
            let sell = { sell with vol = sell.vol -. buy.vol } in
            aux acc sell
    in
    let buys_left =
      buys |> CCList.sort (fun e e' -> Ptime.compare e.time e'.time)
    in
    let init = { pair; yearly_results = IntMap.empty; buys_left } in
    sells
    |> CCList.sort (fun e e' -> Ptime.compare e.time e'.time)
    |> List.fold_left aux init

end

let main () = 
  match Sys.argv |> Array.to_list |> CCList.drop 1 with
  | trades_csv :: [] ->
    let entries = 
      trades_csv
      |> Csv.Rows.load ~has_header:true 
      |> List.map Entry.of_csv_row
      |> List.filter (fun e -> e.margin = 0.)
      (*< Note: margin-trade gains should be calculated from ledger instead*)
    in
    let entries_per_pair =
      entries
      |> List.fold_left (fun acc e ->
        acc |> Pair.Map.update e.pair (function
          | None -> Some [e]
          | Some es -> Some (e::es)
        )
      ) Pair.Map.empty
    in
    entries_per_pair |> Pair.Map.iter (fun pair entries ->
      (* begin *)
      (*   Format.printf "\n\n----- Entries for %a\n%!" Pair.pp pair; *)
      (*   entries *)
      (*   |> CCList.to_string Entry.show *)
      (*   |> print_endline *)
      (* end; *)
      let buys, sells = entries |> CCList.partition (fun e -> e.typ = `Buy) in
      let stats = Stats.calc ~pair ~buys ~sells in
      Format.printf "\n%a\n%!" Stats.pp stats
    )
  | _ ->
    Printf.eprintf "Usage: %s <csv> <year|'all'> <pair|'all'>" (Sys.argv.(0));
    exit 1

let () = main ()
