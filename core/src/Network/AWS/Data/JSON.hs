{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Network.AWS.Data.JSON
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Data.JSON
    (
    -- * FromJSON
      FromJSON (..)
    , parseJSONText
    , eitherDecode
    , eitherDecode'

    -- ** Parser a
    , withObject
    , (.:)
    , (.:?)
    , (.!=)

    -- ** Either String a
    , eitherParseJSON
    , (.:>)
    , (.?>)

    -- * ToJSON
    , ToJSON   (..)
    , toJSONText
    , Value    (Object)
    , object
    , (.=)
    ) where

import           Data.Aeson            (eitherDecode, eitherDecode')
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap     as KeyMap
#else 
import qualified Data.HashMap.Strict   as Map
#endif
import           Data.Aeson.Types
import           Network.AWS.Data.Text

parseJSONText :: FromText a => String -> Value -> Parser a
parseJSONText n = withText n (either fail return . fromText)

toJSONText :: ToText a => a -> Value
toJSONText = String . toText

eitherParseJSON :: FromJSON a => Object -> Either String a
eitherParseJSON = parseEither parseJSON . Object

#if MIN_VERSION_aeson(2,0,0)

(.:>) :: FromJSON a => Object -> Key -> Either String a
(.:>) o k =
    case KeyMap.lookup k o of
        Nothing -> Left $ "key " ++ show k ++ " not present"
        Just v  -> parseEither parseJSON v

(.?>) :: FromJSON a => Object -> Key -> Either String (Maybe a)
(.?>) o k =
    case KeyMap.lookup k o of
        Nothing -> Right Nothing
        Just v  -> parseEither parseJSON v

#else 

(.:>) :: FromJSON a => Object -> Text -> Either String a
(.:>) o k =
    case Map.lookup k o of
        Nothing -> Left $ "key " ++ show k ++ " not present"
        Just v  -> parseEither parseJSON v

(.?>) :: FromJSON a => Object -> Text -> Either String (Maybe a)
(.?>) o k =
    case Map.lookup k o of
        Nothing -> Right Nothing
        Just v  -> parseEither parseJSON v

#endif