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
module System.MIDI.LaunchpadALSA (
  Intensity (..)
  , Color (..)
  , KeyEvent (..)
  , App
  , grid
  , ungrid
  , side
  , makeData
  , sendData
  , getKey
  , listClients
  , findLaunchpad
  , withLaunchpad
  ) where

import           Control.Applicative              ((<$>))
import           Control.Monad                    (join, liftM4, void)
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

-- | Type that withLaunchpad accepts, an IO action that also uses the
-- created handle and connection.
type App = (ALSA.T ALSA.DuplexMode, Connect.T) -> IO ()

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
sendData :: (ALSA.T ALSA.DuplexMode, Connect.T) -> Event.Data -> IO ()
sendData (h, conn) eData = void $ Event.outputDirect (h :: ALSA.T ALSA.DuplexMode) (Event.forConnection conn eData)

-- | Wait for a keypress and then return it.
getKey :: (ALSA.T ALSA.DuplexMode, Connect.T) -> IO KeyEvent
getKey (h, _) = decodeData <$> Event.input h

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
withLaunchpad :: App -> IO ()
withLaunchpad f =
  ALSA.withDefault ALSA.Block $ \h -> do -- Initialize our client, h is Mode
    Port.withSimple h "self" (Port.caps [Port.capRead, Port.capWrite]) Port.typeMidiGeneric $ \selfP -> do --port for self
      maybePinfo <- findLaunchpad h
      let pinfo = fromJust maybePinfo -- info of launchpad
      addr <- PortInfo.getAddr pinfo
      Connect.withTo h selfP addr $ \conn ->
        Connect.withFrom h selfP addr $ \_ ->
          f (h, conn)
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

