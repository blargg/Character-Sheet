{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Frontend.Storage
    ( saveLocal
    , getLocal
    , saveDyn
    , StorageKey(..)
    ) where

import Control.Lens ((^.))
import Data.Text (Text)
import qualified Data.Text as T

import Language.Javascript.JSaddle
import Reflex.Dom

-- Defines all the storage locations for session storage
data StorageKey = Abilities
                | Armor
                | Class
                | Health
                | Skill
                deriving (Show)

-- TODO debounce
-- Save a dynamic value whenever it changes to local storage
saveDyn :: (Show a, PerformEvent t m, MonadJSM (Performable m)) => StorageKey -> Dynamic t a -> m ()
saveDyn key dynVal = performEvent_ (liftJSM . saveLocal key . T.pack <$> serializedValues)
    where serializedValues = show <$> updated dynVal

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
  liftJSM (fromJSVal jsv)

