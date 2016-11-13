-- | Interface to the novation Launchpad through ALSA.
--
-- Unfortunately, the reset, turn on all LED and double buffering keys
-- etcetera seems to require ugly hacks and occasionally doesn't
-- work. Fortunately, this means that the interface is really simple
-- and it should be possible to get up and running very quickly.
module LaunchpadALSA where

import           Control.Applicative              ((<$>))
import           Control.Monad                    (join, liftM4)
import           Data.Maybe                       (catMaybes, fromJust,
                                                   listToMaybe)
import           Data.Word
import qualified Sound.ALSA.Sequencer             as ALSA
import qualified Sound.ALSA.Sequencer.Address     as Address
import qualified Sound.ALSA.Sequencer.Client      as Client
import qualified Sound.ALSA.Sequencer.Client.Info as ClientInfo
import qualified Sound.ALSA.Sequencer.Connect     as Connect
import qualified Sound.ALSA.Sequencer.Event       as Event
import qualified Sound.ALSA.Sequencer.Port        as Port
import qualified Sound.ALSA.Sequencer.Port.Info   as PortInfo
import           Text.Printf                      (printf)


type Mode = ALSA.T ALSA.DuplexMode

data Intensity = Off | Low | Med | High deriving (Show)
data Color = RG Intensity Intensity deriving (Show)

intensityToNum :: Intensity -> Word8
intensityToNum x = case x of
  Off  -> 0
  Low  -> 1
  Med  -> 2
  High -> 3

colorToCode :: Color -> Word8
colorToCode (RG r g) = intensityToNum r + 16 * intensityToNum g

allColors = [RG r g | let l = [Off, Low, Med, High], r <- l, g <- l]
nicepattern = zip allColors [(x, y) | x <- [0..3], y <- [0..3]]
nicepatternx2 = nicepattern ++ map (\(c, (x, y)) -> (c, (7-x, 7-y))) nicepattern
nicepatternx4 = nicepatternx2 ++ map (\(c, (x, y)) -> (c, (7-x, y))) nicepatternx2
nicemessage = map (\(c, (x, y)) -> makeData c (grid x y)) nicepatternx4
donicething = \h conn -> (mapM_ (sendData h conn) nicemessage)
reset = \h conn -> (mapM_ (sendData h conn . uncurry makeData) [(off, grid x y) | x <- [0..7], y <- [0..7]])
one :: Mode -> Connect.T -> IO ()
one = \h conn -> ((sendData h conn . uncurry makeData) ((red, side 1)) >> return ())

green :: Color
green = RG Off High

red :: Color
red = RG High Off

off :: Color
off = RG Off Off

-- x between 0, 8, inclusive. 8 is scene buttons. No mapping for the top buttons afaik.

grid ::  Word8 -> Word8 -> Word8
grid x y | x > 7 || y > 7 = error "Button outside bounds [0..7]"
grid x y = 16 * y + x

side :: Word8 -> Word8
side y = 16 * y + 8

allgrid :: [Word8]
allgrid = [grid x y | x <- [0..7], y <- [0..7]] :: [Word8]


sendData :: Mode -> Connect.T -> Event.Data -> IO Word
sendData h conn eData = Event.outputDirect (h :: Mode) (Event.forConnection conn eData)

-- | Create a sendable piece of data.
makeData
  :: Color -- ^ Address color from Color
  -> Word8 -- ^ Key made by 'grid' or 'side'
  -> Event.Data -- ^ Ready for sending by 'sendData'
makeData color key = Event.NoteEv Event.NoteOn $ Event.simpleNote (Event.Channel 144) (Event.Pitch key) (Event.Velocity $ colorToCode color)

-- | List all the clients for debug purposes
listClients :: IO ()
listClients = do
  putStrLn " Port    Client name                      Port name"
  ALSA.withDefault ALSA.Block $ \h ->
    ClientInfo.queryLoop_ (h :: Mode) $ \cinfo -> do
      client <- ClientInfo.getClient cinfo
      PortInfo.queryLoop_ h client $ \pinfo ->
        join $ liftM4 (printf "%3d:%-3d  %-32.32s %s\n")
          ((\(Client.Cons p) -> p) <$> PortInfo.getClient pinfo)
          ((\(Port.Cons p) -> p) <$> PortInfo.getPort pinfo)
          (ClientInfo.getName cinfo)
          (PortInfo.getName pinfo)

-- | Finds the launchpad if it exists, by our client handle h
findLaunchpad :: Mode -> IO (Maybe PortInfo.T)
findLaunchpad h = do
  l <- ClientInfo.queryLoop (h :: Mode) $ \cinfo -> do
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


resetandlightonkey h conn = do
  print =<< Event.input h
  donicething h conn
  print =<< Event.input h
  reset h conn

main = withLaunchpad resetandlightonkey
