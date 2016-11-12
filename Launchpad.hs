import           Control.Applicative              ((<$>))
import           Control.Monad                    (join, liftM4, unless)
import           Data.Maybe                       (catMaybes, fromJust,
                                                   listToMaybe)
import           Data.Word
import qualified Sound.ALSA.Sequencer             as S
import qualified Sound.ALSA.Sequencer.Address     as A
import qualified Sound.ALSA.Sequencer.Client      as C
import qualified Sound.ALSA.Sequencer.Client.Info as ClientInfo
import qualified Sound.ALSA.Sequencer.Connect     as Conn
import qualified Sound.ALSA.Sequencer.Event       as E
import qualified Sound.ALSA.Sequencer.Port        as P
import qualified Sound.ALSA.Sequencer.Port.Info   as PortInfo
import           Text.Printf                      (printf)


type Mode = S.T S.DuplexMode


-- List all the clients for debug
listClients :: IO ()
listClients = do
  putStrLn " Port    Client name                      Port name"
  S.withDefault S.Block $ \h ->
    ClientInfo.queryLoop_ (h :: Mode) $ \cinfo -> do
      client <- ClientInfo.getClient cinfo
      PortInfo.queryLoop_ h client $ \pinfo ->
        join $ liftM4 (printf "%3d:%-3d  %-32.32s %s\n")
          ((\(C.Cons p) -> p) <$> PortInfo.getClient pinfo)
          ((\(P.Cons p) -> p) <$> PortInfo.getPort pinfo)
          (ClientInfo.getName cinfo)
          (PortInfo.getName pinfo)

-- Finds the launchpad if it exists, by our client handle h
findLaunchpad :: Mode -> IO (Maybe PortInfo.T)
findLaunchpad h = do
  l <- ClientInfo.queryLoop (h :: Mode) $ \cinfo -> do
      client <- ClientInfo.getClient cinfo
      PortInfo.queryLoop h client $ \pinfo -> do
        name <- ClientInfo.getName cinfo
        return $ if name == "Launchpad" then Just pinfo else Nothing
  return . listToMaybe $ catMaybes (concat l)



lupvel :: Word8 -> Word8 -> Word8 -> E.Note
lupvel a b c = E.simpleNote (E.Channel a) (E.Pitch b) (E.Velocity c)

on :: E.Note -> E.Data
on = E.NoteEv E.NoteOn
off :: E.Note -> E.Data
off = E.NoteEv E.NoteOff




test :: Word8 -> Word8 -> Word8 -> IO ()
test a b d =
  S.withDefault S.Block $ \h -> -- Initialize our client, h is Mode
    P.withSimple h "self" (P.caps [P.capRead, P.capSubsRead]) P.typeMidiGeneric $ \selfP -> do --port for self
      maybePinfo <- findLaunchpad h
      let pinfo = fromJust maybePinfo -- info of launchpad
      addr <- PortInfo.getAddr pinfo
      conn <- Conn.createTo h selfP addr
      _ <- E.outputDirect (h :: Mode) (E.forConnection conn (on $ lupvel a b d ))
      return ()

main = test 176 0 126
