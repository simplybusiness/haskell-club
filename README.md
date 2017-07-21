# Haskell Club

The first rule of Haskell Club is that you don't talk about Haskell
Club.  Or at least, not more than, say, five times a day and for
longer than 30 minutes on each occasion.

We haven't decided on rules Two thru Seven

The eighth and final rule is: if it's your first time, you _have_ to
drive.

## Captain's Log

 * [week 1](week-1.org)
 * [week 2](week-2.org)

## Installation

You need ghc and cabal

* On Mac, `brew install ghc cabal-install`
* on Nix, `nix-shell`


Now you can run

```
cabal update
cabal sandbox init
cabal install --only-dependencies
```


## Intellectual property / Commercial secrecy warning 

Simply Business employees heads-up - _this is a public repository_.  If you want to
work with real rating models or other commercially sensitive
intellectual property, this repo would be *exactly the wrong place to
do it*.
