module Launchpad where

import           Control.Applicative              ((<$>))
import           Control.Monad                    (join, liftM4)
import           Data.Maybe                       (catMaybes, listToMaybe)
import qualified Sound.ALSA.Sequencer             as S
import qualified Sound.ALSA.Sequencer.Client      as C
import qualified Sound.ALSA.Sequencer.Client.Info as ClientInfo
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

