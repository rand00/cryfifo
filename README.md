# `cryfifo`

The name stems from
* crying over the FIFO way 
* crypto wins & losses calculated the FIFO way 

Specifically this program was written to support registering wins & losses for 
[taxation in Denmark](https://www.kraken.com/), 
based on csv exports from [Kraken](https://www.kraken.com/).

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

Clone repo:
```
git clone https://github.com/rand00/cryfifo.git
cd cryfifo
```

Install dependencies:
```
opam install csv ptime containers ppx_deriving
```

Compile:
```
dune build
```

Copy the resulting binary somewhere in your `PATH`:
```
cp ./_build/default/bin/cryfifo.exe ~/bin/cryfifo
```


