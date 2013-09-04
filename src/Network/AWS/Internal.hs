{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE RecordWildCards   #-}

{-# OPTIONS_HADDOCK hide       #-}

-- Module      : Network.AWS.Internal
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Internal
    (
    -- * Internal Modules
      module Network.AWS.Internal.Instances
    , module Network.AWS.Internal.Signing
    , module Network.AWS.Internal.String
    , module Network.AWS.Internal.Types

    -- * Convenience
    , module Control.Error
    , module GHC.Generics
    , module Network.HTTP.QueryString.Pickle
    , module Text.XML.Expat.Pickle.Generic

    -- * XML Helpers
    , withNS
    , withNS'
    , withRootNS
    , withRootNS'
    , xmlOptions
    , xpTextContent
    ) where

import Control.Error
import Data.ByteString                 (ByteString)
import Data.Text                       (Text)
import Data.Text.Encoding
import GHC.Generics
import Network.AWS.Internal.Instances
import Network.AWS.Internal.Signing
import Network.AWS.Internal.String
import Network.AWS.Internal.Types
import Network.HTTP.QueryString.Pickle
import Text.XML.Expat.Pickle.Generic

withNS :: ByteString -> XMLGeneric a
withNS ns = withNS' ns $ xmlOptions ns

withNS' :: ByteString -> XMLOptions -> XMLGeneric a
withNS' ns opts = pu { root = (mkNName ns . nnLocalPart) `fmap` (root pu) }
  where
    pu = genericXMLPickler opts

withRootNS :: ByteString -> ByteString -> XMLGeneric a
withRootNS ns name = withRootNS' ns name $ xmlOptions ns

withRootNS' :: ByteString -> ByteString -> XMLOptions -> XMLGeneric a
withRootNS' ns name opts = (genericXMLPickler opts)
   { root = Just $ mkNName ns name
   }

xmlOptions :: ByteString -> XMLOptions
xmlOptions ns = (namespacedXMLOptions ns)
    { xmlListElement = mkNName ns "member"
    }

xpTextContent :: XMLPU [Node] Text
xpTextContent = (decodeUtf8, encodeUtf8) `xpWrap` xpContent xpText
