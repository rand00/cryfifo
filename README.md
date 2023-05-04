# `cryfifo`

The name stems from
* crying over the FIFO way 
* crypto wins & losses calculated the FIFO way 

Specifically this program was written to support registering wins & losses for 
[taxation in Denmark](https://www.kraken.com/), 
based on csv exports from [Kraken](https://www.kraken.com/).

Note that as the FIFO method is stateful, you need to supply the trade/ledger histories from 
the beginning of time - or at least since last time you had no asset at all.

## Usage

```
cryfifo <TRADES.CSV> <LEDGER.CSV>
```

## Features

* supports CSV exports from Kraken, you need to supply both a `trades.csv` and `ledger.csv`
  * the trades is used for FIFO calculation of wins & losses of ordinary trades
    * it is currently also used to get the price of assets near some point in time
  * the ledger is used for margin-trades 
* reports printed via CLI:
  * the FIFO trades statistics 
  * the margin-trades statistics 
  * summed trades statistics 
  * yearly wins & losses
  * yearly summed wins & losses
  * total summed wins & losses
  * yearly summed tax & deduction
  * yearly taxrate equivalent
  * total summed tax & deduction

## Limitations
Currently only supports 
* trades between some crypto asset and euro - not between crypto-assets
* a limited set of assets: ada, xtz, zec

Make issues/PRs on this repository if you want support for something else

## Compiling

First you need to [install the OCaml package manager](https://opam.ocaml.org/doc/Install.html)
`opam`.

Initially setup opam:
```
opam init
opam switch create 4.14.0
```

Clone repo:
```
git clone https://github.com/rand00/cryfifo.git
cd cryfifo
```

Install dependencies:
```
opam install dune csv ptime containers ppx_deriving
```

Compile:
```
dune build
```

Copy the resulting binary somewhere in your `PATH`:
```
cp ./_build/default/bin/cryfifo.exe ~/bin/cryfifo
```


