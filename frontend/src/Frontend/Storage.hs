{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Frontend.Storage
    ( StorageKey(..)
    , loadJson
    , saveDyn
    , saveLocal
    ) where

import Control.Lens ((^.))
import Control.Monad (join)
import Control.Monad.Fix (MonadFix)
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

import Language.Javascript.JSaddle
import Reflex.Dom hiding (decodeText)
import Reflex.Time (debounce)

-- Defines all the storage locations for session storage
data StorageKey = Abilities
                | Armor
                | Class
                | Health
                | Skill
                deriving (Show)

loadJson :: (FromJSON a, MonadJSM m) => StorageKey -> m (Maybe a)
loadJson key = do
    mtext <- liftJSM $ getLocal key
    return . join $ decodeText <$> mtext

-- Save a dynamic value whenever it changes to local storage
saveDyn :: ( ToJSON a
           , MonadFix m
           , MonadHold t m
           , TriggerEvent t m
           , PerformEvent t m
           , MonadJSM (Performable m)
           ) => StorageKey -> Dynamic t a -> m ()
saveDyn key dynVal = do
    debouncedValues <- debounce 2 $ updated dynVal
    let serializedValues = encodeText <$> debouncedValues
    performEvent_ (liftJSM . saveLocal key <$> serializedValues)

encodeText :: ToJSON a => a -> Text
encodeText = TL.toStrict . TL.decodeUtf8 . encode

decodeText :: FromJSON a => Text -> Maybe a
decodeText = decode . TL.encodeUtf8 . TL.fromStrict

saveLocal :: StorageKey -> Text -> JSM ()
saveLocal key msg = do
  jsg ("window" :: Text)
    ^. js ("localStorage" :: Text)
    ^. jss (T.pack . show $ key) [msg]
  return ()

getLocal :: StorageKey -> JSM (Maybe Text)
getLocal key = do
  jsv <- jsg ("window" :: Text)
    ^. js ("localStorage" :: Text)
    ^. js (T.pack . show $ key)
  jsvUndefined <- ghcjsPure (isUndefined jsv)
  jsvNull <- ghcjsPure (isNull jsv)
  if jsvUndefined || jsvNull
     then pure Nothing
     else liftJSM (fromJSVal jsv)

