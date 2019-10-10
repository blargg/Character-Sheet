{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Frontend.Bulma
    ( button
    , indeterminateProgress
    , tabs
    , textInput
    , title
    , subtitle
    ) where

import Control.Lens ((^.))
import Control.Lens.Indexed (iforM_, imapM)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Frontend.Prelude
import Reflex.Dom.Core hiding (textInput, button)

-- basic definition of a materialize styled text input
textInput :: (DomBuilder t m) => Text -> m (Dynamic t Text)
textInput placeholder = do
    let config = def & elConf .~ (classAttr "input is-rounded"
                                 <> "type" =: "text"
                                 <> "placeholder" =: placeholder
                                 )
    textValue <- inputElement config
    return (value textValue)
        where
            elConf = inputElementConfig_elementConfig . elementConfig_initialAttributes

title :: (DomBuilder t m) => Int -> Text -> m ()
title level s = elClass ("h" <> l) ("title is-" <> l) $ text s
    where l = Text.pack (show level)

subtitle :: (DomBuilder t m) => Int -> Text -> m ()
subtitle level s = elClass ("h" <> l) ("subtitle is-" <> l) $ text s
    where l = Text.pack (show level)

button :: (DomBuilder t m) => Text -> m (Event t ())
button t = do
    (e, _) <- elClass' "button" "button" $ text t
    return $ domEvent Click e


indeterminateProgress :: (DomBuilder t m) => m ()
indeterminateProgress = elAttr "progress" ("class" =:"progress is-small is-primary" <> "max" =: "100") $ text "10%"

tabs :: ( MonadFix m
        , DomBuilder t m
        , MonadHold t m
        , PostBuild t m
        , Ord k)
  => Map k (Text, m ()) -- ^ Map from (arbitrary) key to (tab label, child widget)
  -> m ()
tabs elems = tabDisplayFull "tabs is-large" "is-active" elems

-- | Like tab display, with additional options. A widget to construct a tabbed view that shows only one of its child widgets at a time.
--   Creates a header bar containing a <ul> with one <li> per child; clicking a <li> displays
--   the corresponding child and hides all others.
tabDisplayFull :: forall t m k. (MonadFix m, DomBuilder t m, MonadHold t m, PostBuild t m, Ord k)
  => Text               -- ^ Class applied to <div> element around the <ul>
  -> Text       -- ^ Class applied to currently (active, inactive) <li> element
  -> Map k (Text, m ()) -- ^ Map from (arbitrary) key to (tab label, child widget)
  -> m ()
tabDisplayFull divCl activeClass tabItems = do
  let t0 = listToMaybe $ Map.keys tabItems
  rec currentTab :: Demux t (Maybe k) <- elClass "div" divCl $ el "ul" $ do
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
      let attrs = fmap (\b -> if b then Map.singleton "class" activeClass else Map.empty) isSelected
      elDynAttr "li" attrs $ do
        a <- link x
        return $ fmap (const k) (_link_clicked a)
