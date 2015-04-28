## Ardeamon

Ardeamon is a json-rpc server which allows remote communication with an Arduino.

## Build

You can build Ardeamon using cabal.

    cabal sandbox init
    cabal install --only-dependencies
    cabal build

## Usage

The daemon expects an Arduino with a water pump on the digital pin number 3 and
a water level sensor on analog pin number 0, if you don't have the pump and
sensor you can simulate the process with the *--simulate* switch.

    cabal run -- --simulate

See also:

    cabal run -- --help
