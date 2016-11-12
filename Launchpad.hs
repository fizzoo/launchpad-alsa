import           Control.Applicative              ((<$>))
import           Control.Monad                    (join, liftM4)
import           Data.Maybe                       (catMaybes, fromJust,
                                                   listToMaybe)
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

-- Finds the launchpad if it exists
findLaunchpad :: IO (Maybe (C.T, P.T))
findLaunchpad = do
  l <- S.withDefault S.Block $ \h ->
    ClientInfo.queryLoop (h :: Mode) $ \cinfo -> do
      client <- ClientInfo.getClient cinfo
      PortInfo.queryLoop h client $ \pinfo -> do
        name <- ClientInfo.getName cinfo
        c <- PortInfo.getClient pinfo
        port <- PortInfo.getPort pinfo
        return $ if name == "Launchpad" then Just (c, port) else Nothing
  return . listToMaybe $ catMaybes (concat l)



lightupnote :: E.Note
lightupnote = E.simpleNote (E.Channel 144) (E.Pitch 17) (E.Velocity 0x3)
lupvel a b c = E.simpleNote (E.Channel a) (E.Pitch b) (E.Velocity c)
on :: E.Note -> E.Data
on = E.NoteEv E.NoteOn
off = E.NoteEv E.NoteOff




test a b d = do
  S.withDefault S.Block $ \h -> do --client
    selfC <- C.getId h
    P.withSimple h "self" (P.caps [P.capRead, P.capSubsRead]) P.typeMidiGeneric $ \selfP -> do --port for self
      ClientInfo.queryLoop_ (h :: Mode) $ \cinfo -> do -- other client
        client <- ClientInfo.getClient cinfo
        PortInfo.queryLoop_ h client $ \pinfo -> do -- other port
          name <- ClientInfo.getName cinfo
          if name /= "Launchpad" then return () else do
            c <- PortInfo.getClient pinfo
            p <- PortInfo.getPort pinfo
            addr <- PortInfo.getAddr pinfo
            conn <- Conn.createTo h selfP addr
            E.outputDirect (h :: Mode) (E.forConnection conn (on $ lupvel a b d ))
            return ()

main = test 176 0 126
