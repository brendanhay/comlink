{-# LANGUAGE TemplateHaskell #-}

-- Module      : Comlink.Types
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Comlink.Types where

import Control.Lens      (makeLenses)
import Network.SimpleIRC
import Snap

data App = App
    { _irc :: !MIrc
    }

makeLenses ''App

type Comlink = Handler App App
