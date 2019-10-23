{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ConstraintKinds #-}
module Frontend.Storage
    ( StorageKey(..)
    , loadJson
    , saveDyn
    , saveLocal
    , stashValue
    ) where

import Control.Lens ((^.))
import Control.Monad (join)
import Control.Monad.Fix (MonadFix)
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T

import Language.Javascript.JSaddle
import Reflex.Dom hiding (decodeText)
import Reflex.Time (debounce)
import Common.Prelude

import Frontend.Javascript

-- Defines all the storage locations for session storage
data StorageKey = Abilities
                | Armor
                | Attacks
                | Class
                | Health
                | Name
                | Skill
                | Initiative
                | PreparedSpells
                | SpellSets
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

stashValue :: ( FromJSON a
              , ToJSON a
              , MonadJSM m
              , MonadFix m
              , MonadHold t m
              , TriggerEvent t m
              , PerformEvent t m
              , MonadJSM (Performable m)
              )
           => StorageKey
           -> (Maybe a -> m (Dynamic t a))
           -> m (Dynamic t a)
stashValue key mkWidget = do
    initVal <- loadJson key
    dynVal <- mkWidget initVal
    saveDyn key dynVal
    return dynVal

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
  jsvToText jsv
