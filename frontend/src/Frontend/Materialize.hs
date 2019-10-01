{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Frontend.Materialize
    ( tabs
    , initTab
    , textInput
    ) where

import Control.Monad.Fix (MonadFix)
import Control.Lens ((^.))
import Control.Lens.Indexed (iforM_, imapM)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Frontend.Elements as E
import Frontend.Prelude
import Reflex.Dom.Core hiding (textInput)

import Language.Javascript.JSaddle

tabs :: ( MonadFix m
        , DomBuilder t m
        , MonadHold t m
        , PostBuild t m
        , Ord k)
  => Text               -- ^ id for the <ul> element
  -> Map k (Text, m ()) -- ^ Map from (arbitrary) key to (tab label, child widget)
  -> m ()
tabs elId elems = tabDisplayFull "tabs" elId ("tab col s3 red black-text lighten-5", "tab col s3") ("", "") elems

initTab :: Text -> JSM ()
initTab elId = do
  e <- jsg ("document" :: Text)
      ^. js1 ("getElementById" :: Text) elId
  _ <- jsg ("M" :: Text)
      ^. js ("Tabs" :: Text)
      ^. js2 ("init" :: Text) e ()
  _ <- jsg ("console" :: Text)
      ^. js1 ("log" :: Text) e
  _ <- jsg ("console" :: Text)
      ^. js1 ("log" :: Text) ("function ran" :: Text)
  return ()

-- | Like tab display, with additional options. A widget to construct a tabbed view that shows only one of its child widgets at a time.
--   Creates a header bar containing a <ul> with one <li> per child; clicking a <li> displays
--   the corresponding child and hides all others.
tabDisplayFull :: forall t m k. (MonadFix m, DomBuilder t m, MonadHold t m, PostBuild t m, Ord k)
  => Text               -- ^ Class applied to <ul> element
  -> Text               -- ^ id for the <ul> element
  -> (Text, Text)       -- ^ Class applied to currently (active, inactive) <li> element
  -> (Text, Text)       -- ^ Classes applied to (active, inactive) links
  -> Map k (Text, m ()) -- ^ Map from (arbitrary) key to (tab label, child widget)
  -> m ()
tabDisplayFull ulClass ulId (activeClass, inactive) (linkActive, _) tabItems = do
  let t0 = listToMaybe $ Map.keys tabItems
  rec currentTab :: Demux t (Maybe k) <- elAttr "ul" ("class" =: ulClass <> "id" =: ulId) $ do
        tabClicksList :: [Event t k] <- Map.elems <$> imapM (\k (s,_) -> headerBarLink s k $ demuxed currentTab (Just k)) tabItems
        let eTabClicks :: Event t k = leftmost tabClicksList
        fmap demux $ holdDyn t0 $ fmap Just eTabClicks
  el "div" $ do
    iforM_ tabItems $ \k (_, w) -> do
      let isSelected = demuxed currentTab $ Just k
          attrs = ffor isSelected $ \s -> if s then Map.empty else Map.singleton "style" "display:none;"
      elDynAttr "div" attrs w
    return ()
  where
    headerBarLink :: Text -> k -> Dynamic t Bool -> m (Event t k)
    headerBarLink x k isSelected = do
      let attrs = fmap (\b -> if b then Map.singleton "class" activeClass else Map.singleton "class" inactive) isSelected
      elDynAttr "li" attrs $ do
        a <- linkClass x linkActive
        return $ fmap (const k) (_link_clicked a)


-- basic definition of a materialize styled text input
textInput :: (DomBuilder t m) => Text -> m (Dynamic t Text)
textInput elId = E.divC "input-field" $ do
    let config = def & elConf .~ (classAttr "validate"
                                       <> "type" =: "text"
                                  <> "id" =: elId)
    textValue <- inputElement config
    elAttr "label" ("for" =: elId) $ text "search"
    return (value textValue)
        where
            elConf = inputElementConfig_elementConfig . elementConfig_initialAttributes
