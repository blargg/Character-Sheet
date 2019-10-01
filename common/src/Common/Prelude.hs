module Common.Prelude
    ( showT
    ) where

import Data.Text (Text)
import qualified Data.Text as Text

showT :: (Show a) => a -> Text
showT = Text.pack . show
