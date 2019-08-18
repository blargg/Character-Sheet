module Frontend.Javascript
    ( jsvToText
    ) where

import Data.Text (Text)
import Language.Javascript.JSaddle

jsvToText :: JSVal -> JSM (Maybe Text)
jsvToText jsv = do
  jsvUndefined <- ghcjsPure (isUndefined jsv)
  jsvNull <- ghcjsPure (isNull jsv)
  if jsvUndefined || jsvNull
     then pure Nothing
     else liftJSM (fromJSVal jsv)

