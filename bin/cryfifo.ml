
(*example value: 2020-03-14 18:06:46.2255*)
let parse_time str =
  let split = CCString.split_on_char in
  let int = CCInt.of_string_exn in
  let [ date_str; time_str ] = split ' ' str in
  let [ y; m; d ] = split '-' date_str in
  let date = int y, int m, int d in
  let [ h; m; s ] = split ':' time_str in
  let s = match split '.' s with
    | s :: [] 
    | s :: _ :: [] -> s
    | _ -> failwith "parse_time: Failed parsing seconds"
  in
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
      amount : float;
      fee : float;
      refid : string; (*for sanity-checking*)
    }[@@deriving show]

  end
  include T

  let of_csv_row row =
    let open CCOption.Infix in
    let r k = Csv.Row.find row k in
    r "type" |> Typ.of_string >>= fun typ -> 
    r "asset" |> Asset.of_string >>= fun asset -> 
    let time = parse_time @@ r "time" in
    let amount = Float.of_string @@ r "amount" in
    let fee = Float.of_string @@ r "fee" in
    let refid = r "refid" in
    Some { time; typ; asset; amount; fee; refid }
  
end

(*Hmm - needed for printing maps properly*)
let pp_start x () = Format.fprintf x "@[<v 4>    "
let pp_arrow x () = Format.fprintf x " -> @["
let pp_sep x () = Format.fprintf x ";@]@,"
let pp_stop x () = Format.fprintf x "@]@]"

module IntMap = struct
  include CCMap.Make(CCInt)
  let pp pp_v = pp ~pp_start ~pp_sep ~pp_arrow ~pp_stop CCInt.pp pp_v
end

let year_of_time t =
  let y, _, _ = Ptime.to_date t in
  y

module Stats = struct 

  module T = struct 

    type yearly_result = {
      wins : float;
      losses : float;
    }[@@deriving show]

    type t = {
      asset : Asset.t;
      yearly_results : yearly_result IntMap.t;
      buys_left : TradeEntry.t list;
    }[@@deriving show]

  end
  include T

  let update_yearly_results ~year ~wins ~losses v =
    v |> IntMap.update year (function
      | None -> Some { wins; losses }
      | Some yres -> 
        let wins, losses = yres.wins +. wins, yres.losses in
        Some { wins; losses }
    )

  (*Warning; depends on margin-trades being filtered away*)
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

let to_eur ~asset ~time ~trades amount =
  if asset = `ZEUR then amount else 
    let closest_trade = 
      trades |> List.fold_left (fun acc_closest_trade trade ->
        match acc_closest_trade with
        | None ->
          if not (trade.TradeEntry.asset = asset) then
            None
          else 
            Some trade
        | Some acc_closest_trade -> 
          if not (trade.TradeEntry.asset = asset) then
            Some acc_closest_trade
          else
            let diff_acc = Ptime.diff acc_closest_trade.time time |> Ptime.Span.abs in
            let diff_trade = Ptime.diff trade.time time |> Ptime.Span.abs in
            if Ptime.Span.compare diff_trade diff_acc < 0 then
              Some trade
            else
              Some acc_closest_trade
      ) None
    in
    match closest_trade with
    | Some t -> t.price *. amount
    | None ->
      Format.eprintf "ERROR: to_eur: No trade found of asset %a" Asset.pp asset;
      exit 1

(*> Warning; this depends on ledger only containing `Margin and `Rollover entries*)
let margin_stats_of_ledger trades ledger : Stats.t Asset.Map.t =
  let open Stats.T in
  let open LedgerEntry.T in
  let calc_wins_losses asset ledger_entry =
    let time = ledger_entry.time in
    let amount_eur = ledger_entry.amount |> to_eur ~asset ~time ~trades in
    let fee_eur = ledger_entry.fee |> to_eur ~asset ~time ~trades in
    let wins = if amount_eur > 0. then amount_eur else 0. in
    let losses =
      (*goto possibly subtract fees from wins instead - test effect
        .. see what danish SKAT says about this
      *)
      (if amount_eur <= 0. then -. amount_eur else 0.) +. fee_eur
    in
    { wins; losses }
  in
  ledger
  |> List.fold_left (fun acc_asset_map ledger_entry ->
    let asset = ledger_entry.LedgerEntry.asset in
    acc_asset_map
    |> Asset.Map.update asset (function
      | None ->
        let yres = calc_wins_losses asset ledger_entry in
        let yearly_results =
          IntMap.singleton (year_of_time ledger_entry.time) yres
        in
        Some { asset; yearly_results; buys_left = [] }
      | Some stats ->
        let res = calc_wins_losses asset ledger_entry in
        let yearly_results =
          IntMap.update (year_of_time ledger_entry.time) (function
            | None -> Some res
            | Some yres ->
              let wins = res.wins +. yres.wins in
              let losses = res.losses +. yres.losses in
              Some { wins; losses }
          ) stats.yearly_results
        in
        Some { stats with yearly_results }
    )
  ) Asset.Map.empty

let main () = 
  match Sys.argv |> Array.to_list |> CCList.drop 1 with
  | trades_csv :: ledgers_csv :: [] ->
    let trades =
      trades_csv
      |> Csv.Rows.load ~has_header:true 
      |> List.map TradeEntry.of_csv_row
    in
    let trades_without_margins = 
      trades
      |> List.filter (fun e -> e.margin = 0.)
    in
    let trades_per_asset =
      trades_without_margins
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
      |> List.filter_map LedgerEntry.of_csv_row
    in
    let margin_trades_stats = margin_stats_of_ledger trades ledger in
    let all_stats = add_stats_assetmaps
        trades_stats
        margin_trades_stats
    in
    let pp_stats = Asset.Map.pp ~pp_start ~pp_sep ~pp_arrow ~pp_stop
        Asset.pp Stats.pp in
    Format.printf "\n@[trades@ stats@ =@]@,%a@." pp_stats trades_stats;
    Format.printf "\n@[margin@ trades@ stats@ =@]@,%a@." pp_stats margin_trades_stats;
    Format.printf "\n@[summed stats@ =@]@,%a@." pp_stats all_stats;
    print_endline "\nyearly summed wins/losses per asset:";
    all_stats |> Asset.Map.iter (fun asset stats ->
      Format.printf "    %a\n%!" Asset.pp asset;
      stats.Stats.T.yearly_results |> IntMap.iter (fun year yres ->
        Format.printf "        %d -> %.4f eur\n%!"
          year (yres.Stats.T.wins -. yres.losses)
      );
    );
    let yearly_wins_losses = 
      let open Stats.T in
      Asset.Map.fold (fun asset stats acc ->
        IntMap.fold (fun year yres acc ->
          IntMap.update year (function
            | None -> Some yres
            | Some acc_yres ->
              let acc_yres = {
                wins = acc_yres.wins +. yres.wins;
                losses = acc_yres.losses +. yres.losses;
              } in
              Some acc_yres
          ) acc
        ) stats.Stats.T.yearly_results acc
      ) all_stats IntMap.empty
    in
    Format.printf "\nyearly@ wins/losses@ =@ @,%a@."
      (IntMap.pp Stats.pp_yearly_result) yearly_wins_losses;
    let yearly_summed_wins_losses = 
      let open Stats.T in
      Asset.Map.fold (fun asset stats acc ->
        IntMap.fold (fun year yres acc ->
          IntMap.update year (function
            | None ->
              let sum = yres.wins -. yres.losses in
              Some sum
            | Some acc_sum ->
              let acc_sum = acc_sum +. (yres.wins -. yres.losses) in
              Some acc_sum
          ) acc
        ) stats.Stats.T.yearly_results acc
      ) all_stats IntMap.empty
    in
    let pp_yearly_sums =
      let pp_float f = Format.fprintf f "%.4f" in
      IntMap.pp (* ~pp_start ~pp_sep ~pp_arrow ~pp_stop *) pp_float in
    Format.printf "\nyearly@ summed@ wins/losses@ =@ @,%a@."
      pp_yearly_sums yearly_summed_wins_losses;
    let summed_wins_losses =
      Asset.Map.fold (fun asset stats acc ->
        IntMap.fold (fun year yres acc ->
          acc +. yres.Stats.T.wins -. yres.Stats.T.losses
        ) stats.Stats.T.yearly_results acc
      ) all_stats 0.
    in
    Format.printf "\ntotal summed wins/losses = %.4f eur\n%!"
      summed_wins_losses;
    let calc_danish_tax_negtax yres =
      let danish_max_tax = 0.53 in
      let tax = danish_max_tax *. yres.Stats.T.wins in
      let neg_tax =
        (*> 6600 dkk = 880 eur, see https://skat.dk/data.aspx?oid=2237057&year=2022&layout=2503*)
        if yres.Stats.T.losses > 880. then
          0.26 *. (yres.Stats.T.losses -. 880.)
        else
          0.
      in
      tax, neg_tax
    in
    let yearly_summed_tax_negtax =
      Asset.Map.fold (fun asset stats acc ->
        IntMap.fold (fun year yres acc ->
          let tax, neg_tax = calc_danish_tax_negtax yres in
          acc |> IntMap.update year (function
            | None -> 
              Some (tax -. neg_tax)
            | Some acc_eur -> 
              Some (acc_eur +. tax -. neg_tax)
          )
        ) stats.Stats.T.yearly_results acc
      ) all_stats IntMap.empty
    in
    Format.printf "\nyearly summed tax/deduction = @,%a@."
      pp_yearly_sums yearly_summed_tax_negtax;
    let yearly_taxrate_equivalent =
      IntMap.mapi (fun year summed_wins_losses ->
        if summed_wins_losses = 0. then 0. else 
          let taxrate =
            IntMap.get_or ~default:0. year yearly_summed_tax_negtax
            /. summed_wins_losses
          in
          taxrate *. 100.
        ) yearly_summed_wins_losses
    in
    Format.printf "\nyearly taxrate equivalent = @,%a@."
      pp_yearly_sums yearly_taxrate_equivalent;
    let summed_tax_negtax =
      Asset.Map.fold (fun asset stats acc ->
        IntMap.fold (fun year yres acc ->
          let tax, neg_tax = calc_danish_tax_negtax yres in
          acc +. tax -. neg_tax
        ) stats.Stats.T.yearly_results acc
      ) all_stats 0.
    in
    Format.printf "\ntotal summed tax/deduction = %.4f eur\n%!"
      summed_tax_negtax;
    print_newline ();
    print_endline "NOTE: Wins/losses are calculated via the FIFO method";
    print_endline "NOTE: Tax-rate is set to the danish max of 53%";
    print_endline "NOTE: Deduction first subtracts 800. eur from losses (danish law)";
    print_endline "NOTE: Deduction-rate on losses is set to the danish 26%";
    print_newline ();
  | _ ->
    Printf.eprintf "Usage: %s <trades.csv> <ledgers.csv>\n%!" (
      Filename.basename Sys.argv.(0)
    );
    exit 1

let () = main ()
