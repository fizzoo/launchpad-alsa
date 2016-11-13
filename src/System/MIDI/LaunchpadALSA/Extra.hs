-- | Samples and other extras for LaunchpadALSA
--
-- You will want to import qualified or otherwise only what you want
-- since main is defined, amongst other things.
module System.MIDI.LaunchpadALSA.Extra where

import           System.MIDI.LaunchpadALSA

import           Control.Monad              (forever)
import           Data.Word
import qualified Sound.ALSA.Sequencer.Event as Event (Data)

-- * Extras & Examples

-- | Sample 'App' that turns on all the lights in a pretty pattern on
-- the first event, resets all the lights on the second event (key up
-- most likely), and then quits.
resetAndLightOnKey :: App
resetAndLightOnKey h conn = do
  print  =<< getKey h
  drawNicePattern h conn
  print =<< getKey h
  reset h conn

-- | Sample 'App' that keeps lighting the LEDs on keypresses.
keepLighting :: App
keepLighting h conn = forever $ resetAndLightOnKey h conn

-- | Sample 'App' that lights up the key that is pressed (except top
-- buttons since they're unaddressable). Pressing the mixer button exits.
lightPressed :: App
lightPressed h conn = do
  d <- getKey h
  let cmd = case d of
        Down x y -> if x < 9 then Just $ makeData red (grid x y) else Nothing
        Up x y   -> if x < 9 then Just $ makeData off (grid x y) else Nothing
  mapM_ (sendData h conn) cmd
  case d of
    Down 9 7 -> return ()
    _        -> lightPressed h conn

-- | Sample main with above 'App'.
main :: IO ()
main = withLaunchpad lightPressed

-- | Keys for all the grid buttons.
allGrid :: [Word8]
allGrid = [grid x y | x <- [0..7], y <- [0..7]] :: [Word8]

-- | Keys for all the side buttons.
allSide :: [Word8]
allSide = [side y | y <- [0..7]]

-- | List of all the possible color values (16).
allColors :: [Color]
allColors = [RG r g | let l = [Off, Low, Med, High], r <- l, g <- l]

-- | A nice circular pattern.
nicePattern :: [Event.Data]
nicePattern = map (\(c, (x, y)) -> makeData c (grid x y)) nicepatternx4
  where
    nicepatternx4 = nicepatternx2 ++ map (\(c, (x, y)) -> (c, (7-x, y))) nicepatternx2
    nicepatternx2 = nicepatternx1 ++ map (\(c, (x, y)) -> (c, (7-x, 7-y))) nicepatternx1
    nicepatternx1 = zip allColors [(x, y) | x <- [0..3], y <- [0..3]]

-- | 'App' that draws colors from the centre of the grid.
drawNicePattern :: App
drawNicePattern h conn = mapM_ (sendData h conn) nicePattern

-- | 'App' that resets all the LEDs.
reset :: App
reset h conn = mapM_ (sendData h conn . uncurry makeData) [(off, grid x y) | x <- [0..7], y <- [0..7]]

-- | Basic color green.
green :: Color
green = RG Off High

-- | Basic color red.
red :: Color
red = RG High Off

-- | Color value for off.
off :: Color
off = RG Off Off
