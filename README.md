About
=====
A very simple tool for getting the average market price of bitcoins using data
from [preev.com](http://preev.com/).

It might be cool for haskell noobs, such as myself, to look at the source code
(a single file) and see how to parse both json (using aeson) and command line
arguments (using optparse-applicative)!

Install with <code>$ cabal install</code>. Use a sandbox to remain somewhat
sane.

Usage
=====
```
~ $ preev --help
Gets the average market price for any amount of BTC from several exchanges weighted by volume

Usage: preev AMOUNT [-t|--to CURRENCY] [-s|--source EXCHANGES] [-v|--verbose]
  Average market price for AMOUNT bitcoins

Available options:
  -h,--help                Show this help text
  -t,--to CURRENCY         Target currency (e.g. USD, EUR, SEK, NOK) defaults to USD
  -s,--source EXCHANGES    Comma separated (no whitespaces) list of sources (e.g. bitstamp,btce)
  -v,--verbose             Enable verbose mode

~ $ preev 0.24 --verbose
1 BTC = 635.00 USD on bitstamp
1 BTC = 629.00 USD on btce
1 BTC = 677.10 USD on localbitcoins
-------
0.24 BTC are on average (weighted by volume) worth 152.08 USD

~ $ preev 0.24 --to sek --verbose
1 BTC = 4085.00 SEK on bitstamp
1 BTC = 3996.00 SEK on btce
1 BTC = 4308.00 SEK on localbitcoins
-------
0.24 BTC are on average (weighted by volume) worth 973.69 SEK

~ $ preev 0.24 --to sek
973.69

~ $ cool!
cool!: command not found
```

Disclaimer
==========
It's a hack. Your head might [a splode](http://www.homestarrunner.com/sbemail94.html)!