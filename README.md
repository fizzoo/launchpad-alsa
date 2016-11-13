# LaunchpadALSA

Haskell package for easily controlling the LEDs and taking input from
the first Novation Launchpad.  (Unsure of the support for the other
versions, though it shouldn't be any difficult to add if I get some
possibility of testing it.)

Look in the Extra directory for samples, a quick test would be to use
the provided main there.

A complete program to light up the top-left LED and then quit on
pressing mixer:

```haskell
import           System.MIDI.LaunchpadALSA
import           System.MIDI.LaunchpadALSA.Extra (off, red)

app :: App
app h conn = do
  sendData h conn $ makeData red (grid 0 0)
  key <- getKey h
  case key of
    Down 9 7 -> sendData h conn $ makeData off (grid 0 0)
    _        -> app h conn

main :: IO ()
main = withLaunchpad app
```

## Install

```shell
git clone https://github.com/fizzoo/launchpad-alsa
cd launchpad-alsa
cabal install
```
