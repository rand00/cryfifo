
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

module Asset = struct 

  module T = struct 

    (*goto define more asset types*)
    type t = [ `XTZ | `ZEUR | `ADA | `XZEC ]
    [@@deriving show,eq,ord]

  end
  include T

  let of_string : string -> t option = function
    | "XZEC" -> Some `XZEC
    | "XTZ" -> Some `XTZ
    | "ADA" -> Some `ADA
    | "ZEUR" -> Some `ZEUR
    | _ -> None

  (*goto - currently only trades from crypto->eur is supported
    .. generally should support any pair,
    .. and transformation to a base currency
       .. which for correctness would need to be looked up from a timeseries
  *)
  let of_asset_pair_string : string -> t = function
    | "XZECZEUR" -> `XZEC
    | "ADAEUR" -> `ADA
    | "XTZEUR" -> `XTZ
    | s -> failwith @@ "Error parsing pair: '"^s^"'"

  module Map = CCMap.Make(T)

end

module TradeEntry = struct 

  module Typ = struct 

    type t = [ `Buy | `Sell ]
    [@@deriving show]

    let of_string = function
      | "buy" -> `Buy
      | "sell" -> `Sell
      | s -> failwith @@ "Error parsing typ: '"^s^"'"

  end

  module T = struct 
  
    type t = {
      time : Ptime.t;
      asset : Asset.t;
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
    let asset = Asset.of_asset_pair_string @@ r "pair" in
    let typ = Typ.of_string @@ r "type" in
    let price = Float.of_string @@ r "price" in
    let fee = Float.of_string @@ r "fee" in
    let margin = Float.of_string @@ r "margin" in
    let vol = Float.of_string @@ r "vol" in
    { time; asset; typ; price; fee; margin; vol }
  
end

open TradeEntry.T

module LedgerEntry = struct 

  module Typ = struct 

    type t = [ `Margin | `Rollover ]
    [@@deriving show]

    let of_string = function
      | "margin" -> Some `Margin
      | "rollover" -> Some `Rollover
      | _ -> None

  end

  module T = struct 
  
    type t = {
      time : Ptime.t;
      typ : Typ.t;
      asset : Asset.t;
      amount : float; (*eur*)
      fee : float; (*eur*)
      refid : string; (*for sanity-checking*)
    }[@@deriving show]

  end
  include T

  let of_csv_row row =
    let open CCOption.Infix in
    begin
      let r k = Csv.Row.find row k in
      r "type" |> Typ.of_string >>= fun typ -> 
      r "asset" |> Asset.of_string >>= fun asset -> 
      let time = parse_time @@ r "time" in
      let amount = Float.of_string @@ r "amount" in
      let fee = Float.of_string @@ r "fee" in
      let refid = r "refid" in
      Some { time; typ; asset; amount; fee; refid }
    end |> CCOption.get_exn_or "Error parsing LedgerEntry"
  
end

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
    asset : Asset.t;
    yearly_results : yearly_result IntMap.t;
    buys_left : TradeEntry.t list;
  }[@@deriving show]

  let update_yearly_results ~year ~wins ~losses v =
    v |> IntMap.update year (function
      | None -> Some { wins; losses }
      | Some yres -> 
        let wins, losses = yres.wins +. wins, yres.losses in
        Some { wins; losses }
    )

  let of_trades ~asset ~buys ~sells =
    let open TradeEntry.T in
    let rec aux acc sell =
      match acc.buys_left with
      | [] ->
        (*Note: this can happen if staking gave extra volume*)
        Format.eprintf "WARNING: There is no buys left for %a - \
                        selling pot. staked volume %.2f eur\n%!"
          Asset.pp asset
          (sell.vol *. sell.price);
        let year = year_of_time sell.time in
        let wins, losses = sell.vol *. sell.price, 0. in
        let yearly_results =
          acc.yearly_results
          |> update_yearly_results ~year ~wins ~losses
        in
        { acc with yearly_results }
      | buy :: buys_left ->
        if Ptime.is_earlier sell.time ~than:buy.time then (
          Format.eprintf "WARNING: sell time (%a) is earlier than buy time (%a) \
                          -- registering sell as win %a\n%!"
            Ptime.pp sell.time 
            Ptime.pp buy.time 
            TradeEntry.pp buy;
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
    let init = {
      asset = asset;
      yearly_results = IntMap.empty;
      buys_left
    } in
    sells
    |> CCList.sort (fun e e' -> Ptime.compare e.time e'.time)
    |> List.fold_left aux init

  let add x y =
    assert (x.asset = y.asset);
    let asset = x.asset in
    let buys_left = x.buys_left @ y.buys_left in
    let yearly_results =
      let f _year = function
        | `Both (x_yres, y_yres) ->
          let yres = {
            wins = x_yres.wins +. y_yres.wins;
            losses = x_yres.losses +. y_yres.losses;
          } in
          Some yres
        | `Left yres -> Some yres
        | `Right yres -> Some yres
      in
      IntMap.merge_safe ~f x.yearly_results y.yearly_results
    in
    { asset; yearly_results; buys_left }
  
end

let add_stats_assetmaps =
  let f _asset = function
    | `Both (x_stats, y_stats) -> Some (Stats.add x_stats y_stats)
    | `Left stats | `Right stats -> Some stats
  in
  Asset.Map.merge_safe ~f

(*goto howto
  * fold over ledger-entries
    * accumulating over asset -> stats map
      * accumulating over stats
        * add all positive amounts to wins
        * add all negative amounts to losses 
        * subtract fees from corresponding wins/losses
          * use refid for finding correspondents
  * note: ledger-entries need to include `ZEUR as well as crypto asset
*)
let margin_stats_of_ledger ledger =
  failwith "todo"

let main () = 
  match Sys.argv |> Array.to_list |> CCList.drop 1 with
  | trades_csv :: ledgers_csv :: [] ->
    let trades_per_asset =
      trades_csv
      |> Csv.Rows.load ~has_header:true 
      |> List.map TradeEntry.of_csv_row
      |> List.filter (fun e -> e.margin = 0.)
      (*< Note: margin-trade gains should be calculated from ledger instead*)
      |> List.fold_left (fun acc e ->
        acc |> Asset.Map.update e.asset (function
          | None -> Some [e]
          | Some es -> Some (e::es)
        )
      ) Asset.Map.empty
    in
    let trades_stats =
      trades_per_asset |> Asset.Map.mapi (fun asset trade_entries ->
        let buys, sells =
          trade_entries |> CCList.partition (fun e -> e.typ = `Buy)
        in
        Stats.of_trades ~asset ~buys ~sells
      )
    in
    let ledger =
      ledgers_csv
      |> Csv.Rows.load ~has_header:true 
      |> List.map LedgerEntry.of_csv_row
    in
    let margin_trades_stats = margin_stats_of_ledger ledger in
    let all_stats = add_stats_assetmaps
        trades_stats
        margin_trades_stats
    in
    let pp_stats = Asset.Map.pp Asset.pp Stats.pp in
    Format.printf "\ntrades stats = %a\n%!" pp_stats trades_stats;
    Format.printf "\nmargin trades stats = %a\n%!" Stats.pp margin_trades_stats;
    Format.printf "\nsummed stats = %a\n%!" pp_stats all_stats
  | _ ->
    Printf.eprintf "Usage: %s <trades.csv> <ledgers.csv>" (Sys.argv.(0));
    exit 1

let () = main ()
