{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

-- Module      : Main
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Main (main) where

import           Control.Concurrent        (threadDelay)
import           Control.Exception         (throw)
import           Control.Lens              (makeLenses, view)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Char8     as BS
import           Data.Char
import           Data.List.Split
import           Data.Maybe
import           Data.Predicate
import           Network.HTTP.Types.Status
import           Network.SimpleIRC
import           Snap
import           Snap.Predicate            hiding (Error, Text)
import           Snap.Route                hiding (connect)
import           System.Environment

data App = App
    { _irc :: !MIrc
    }

makeLenses ''App

type Comlink = Handler App App

main :: IO ()
main = do
    host  <- getEnv "IRC_HOST"
    port  <- getEnv "IRC_PORT"
    name  <- getEnv "IRC_NICK"
    chans <- splitOn "," <$> getEnv "IRC_CHANNELS"
    dbg   <- isJust <$> lookupEnv "IRC_DEBUG"

    let cfg = (mkDefaultConfig host name)
            { cUsername    = name
            , cRealname    = name
            , cChannels    = chans
            , cEvents      = events chans
            , cPort        = 6667
            , cCTCPVersion = name
            }

    either throw (serveSnaplet defaultConfig . initialise) =<<
        connect cfg True dbg

initialise :: MIrc -> SnapletInit App App
initialise mirc = makeSnaplet "comlink" "Comlink" Nothing $ do
    addRoutes $ expandRoutes sitemap
    return $! App mirc

sitemap :: Routes Comlink ()
sitemap =
    post "/notify" notify $ Accept Application FormUrlEncoded
       :&: Param "level"
       :&: Param "event"
       :&: Param "host"
       :&: Param "message"

notify :: MediaType Application FormUrlEncoded :*: ByteString :*: ByteString  :*: ByteString  :*: ByteString
       -> Comlink ()
notify (_ :*: lvl :*: evt :*: host :*: msg) = do
    mirc <- view irc
    liftIO $ do
        let payload = format lvl evt host msg
        mapM_ (\c -> sendMsg mirc c payload) =<< getChannels mirc
    respond status200

format :: ByteString -> ByteString -> ByteString -> ByteString -> ByteString
format lvl evt host msg = BS.concat
    [ "["
    , BS.map normalise lvl
    , "] "
    , "["
    , BS.map normalise evt
    , "] "
    , "<"
    , host
    , "> "
    , msg
    ]
  where
    normalise ' '   = '_'
    normalise c
        | isUpper c = toLower c
        | otherwise = c

respond :: MonadSnap m => Status -> m ()
respond (Status c m) = modifyResponse $ setResponseStatus c m

events :: [String] -> [IrcEvent]
events cs =
    [ Part kicked
    , Kick kicked
    , Disconnect disconnected
    ]
  where
    kicked s _ = delay >> mapM_ (\c -> sendCmd s $ MJoin c Nothing) chans

    disconnected s = do
        res <- reconnect s
        either (\e -> print e >> delay >> disconnected s)
               (const $ return ()) res

    delay = threadDelay 10000000
    chans = map BS.pack cs
