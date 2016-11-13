-- | Interface to the novation Launchpad through ALSA.
--
-- Unfortunately, the reset, turn on all LED and double buffering keys
-- etcetera seems to require ugly hacks and occasionally doesn't
-- work. Fortunately, this means that the interface is really simple
-- and it should be possible to get up and running very quickly.
--
-- Occasionally when exiting and resuming some program using the
-- launchpad we don't receive any events. A very simple workaround is
-- that, depending on what caused the problem the round buttons to the
-- right or up work. Pushing vol and mixer, for example, should
-- therefore always fix that. Sometimes waiting for a few seconds
-- seems to work also. I suspect this is more annoying while debugging
-- than it will be in use.
module LaunchpadALSA where

import           Control.Applicative              ((<$>))
import           Control.Monad                    (forever, join, liftM4, void,
                                                   when)
import           Data.Maybe                       (catMaybes, fromJust,
                                                   listToMaybe)
import           Data.Tuple                       (swap)
import           Data.Word
import qualified Sound.ALSA.Sequencer             as ALSA
import qualified Sound.ALSA.Sequencer.Client      as Client
import qualified Sound.ALSA.Sequencer.Client.Info as ClientInfo
import qualified Sound.ALSA.Sequencer.Connect     as Connect
import qualified Sound.ALSA.Sequencer.Event       as Event
import qualified Sound.ALSA.Sequencer.Port        as Port
import qualified Sound.ALSA.Sequencer.Port.Info   as PortInfo
import           Text.Printf                      (printf)


-- | Intensity of a color, 4 possible values.
data Intensity = Off | Low | Med | High deriving (Show)

-- | A color, in intensity of red and green.
data Color = RG Intensity Intensity deriving (Show)

-- | A key that was pressed on the launchpad.
-- x, y is like in grid/ungrid, but top buttons have x of 9.
data KeyEvent = Down Word8 Word8 | Up Word8 Word8 deriving (Show)

-- | Key for the grid where x,y in 0..7, corresponding to x,y from
-- top-left corner.  If x is 8 the y corresponds to the side buttons
-- instead.  No available mapping for the top buttons afaik.
grid ::  Word8 -> Word8 -> Word8
grid x y | x > 8 || y > 7 = error "Button outside bounds x in [0..8], y in [0..7]"
grid x y = 16 * y + x

-- | Key for side button.
side :: Word8 -> Word8
side = grid 8

-- | Create a sendable piece of data.
makeData
  :: Color -- ^ Address color from Color
  -> Word8 -- ^ Key made by 'grid' or 'side'
  -> Event.Data -- ^ Ready for sending by 'sendData'
makeData color key = Event.NoteEv Event.NoteOn $ Event.simpleNote (Event.Channel 144) (Event.Pitch key) (Event.Velocity $ colorToCode color)

-- | Send data on connection.
sendData :: ALSA.T ALSA.DuplexMode -> Connect.T -> Event.Data -> IO ()
sendData h conn eData = void $ Event.outputDirect (h :: ALSA.T ALSA.DuplexMode) (Event.forConnection conn eData)

-- | Gets the x and y positions from a key.
ungrid :: Word8 -> (Word8, Word8)
ungrid key = swap $ divMod key 16

-- | Decode the Event into a KeyEvent.
decodeData :: Event.T -> KeyEvent
decodeData e = let
    body = Event.body e
    normalizectrl a = (a - 104) * 16 + 9
    pitch = case body of
      Event.NoteEv _ a -> Event.unPitch $ Event.noteNote a
      Event.CtrlEv _ a -> fromIntegral $ normalizectrl $ Event.unParameter $ Event.ctrlParam a
      _                -> error "Weird button pressed"
    vel = case body of
      Event.NoteEv _ a -> Event.unVelocity $ Event.noteVelocity a
      Event.CtrlEv _ a -> fromIntegral $ Event.unValue $ Event.ctrlValue a
      _                -> error "Weird button pressed"
    (x, y) = ungrid pitch
    in if vel > 64
       then Down x y
       else Up x y


-- | List all the clients for debug purposes
-- (From example of ALSA)
listClients :: IO ()
listClients = do
  putStrLn " Port    Client name                      Port name"
  ALSA.withDefault ALSA.Block $ \h ->
    ClientInfo.queryLoop_ (h :: ALSA.T ALSA.DuplexMode) $ \cinfo -> do
      client <- ClientInfo.getClient cinfo
      PortInfo.queryLoop_ h client $ \pinfo ->
        join $ liftM4 (printf "%3d:%-3d  %-32.32s %s\n")
          ((\(Client.Cons p) -> p) <$> PortInfo.getClient pinfo)
          ((\(Port.Cons p) -> p) <$> PortInfo.getPort pinfo)
          (ClientInfo.getName cinfo)
          (PortInfo.getName pinfo)

-- | Finds the launchpad if it exists, by our client handle h
findLaunchpad :: ALSA.T ALSA.DuplexMode -> IO (Maybe PortInfo.T)
findLaunchpad h = do
  l <- ClientInfo.queryLoop (h :: ALSA.T ALSA.DuplexMode) $ \cinfo -> do
      client <- ClientInfo.getClient cinfo
      PortInfo.queryLoop h client $ \pinfo -> do
        name <- ClientInfo.getName cinfo
        return $ if name == "Launchpad" then Just pinfo else Nothing
  return . listToMaybe $ catMaybes (concat l)

-- | Finds the launchpad, and executes f with a client handle and
-- connection. Could be used for one-shots, but it recreates a client
-- and gets the connection with each call. Compose your main to use
-- the given parameters instead, in order to keep the connection open.
withLaunchpad :: (ALSA.T ALSA.DuplexMode -> Connect.T -> IO ()) -> IO ()
withLaunchpad f =
  ALSA.withDefault ALSA.Block $ \h -> do -- Initialize our client, h is Mode
    Port.withSimple h "self" (Port.caps [Port.capRead, Port.capWrite]) Port.typeMidiGeneric $ \selfP -> do --port for self
      maybePinfo <- findLaunchpad h
      let pinfo = fromJust maybePinfo -- info of launchpad
      addr <- PortInfo.getAddr pinfo
      Connect.withTo h selfP addr $ \conn ->
        Connect.withFrom h selfP addr $ \_ ->
          f h conn
    return ()


-- * Internals

-- | Creates the (to be shifted) bit pattern of a certain intensity.
intensityToNum :: Intensity -> Word8
intensityToNum x = case x of
  Off  -> 0
  Low  -> 1
  Med  -> 2
  High -> 3

-- | Creates the launchpad color code for the input color.
colorToCode :: Color -> Word8
colorToCode (RG r g) = intensityToNum r + 16 * intensityToNum g

-- * Extras & Examples

-- | Type that withLaunchpad accepts, an IO action that also uses the
-- created handle and connection.
type App = ALSA.T ALSA.DuplexMode -> Connect.T -> IO ()

-- | Sample 'App' that turns on all the lights in a pretty pattern on
-- the first event, resets all the lights on the second event (key up
-- most likely), and then quits.
resetAndLightOnKey :: App
resetAndLightOnKey h conn = do
  print . decodeData =<< Event.input h
  drawNicePattern h conn
  print . decodeData =<< Event.input h
  reset h conn

-- | Sample 'App' that keeps lighting the LEDs on keypresses.
keepLighting :: App
keepLighting h conn = forever $ resetAndLightOnKey h conn

-- | Sample 'App' that lights up the key that is pressed (except top
-- buttons since they're unaddressable). Pressing the mixer button exits.
lightPressed :: App
lightPressed h conn = do
  inp <- Event.input h
  let d = decodeData inp
      cmd = case d of
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
