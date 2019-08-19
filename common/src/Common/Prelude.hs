module Common.Prelude
    ( (=:)
    , decodeText
    , encodeText
    , showT
    ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Aeson
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

import Reflex.Dom.Core ((=:))

showT :: (Show a) => a -> Text
showT = Text.pack . show

encodeText :: ToJSON a => a -> Text
encodeText = TL.toStrict . TL.decodeUtf8 . encode

decodeText :: FromJSON a => Text -> Maybe a
decodeText = decode . TL.encodeUtf8 . TL.fromStrict

