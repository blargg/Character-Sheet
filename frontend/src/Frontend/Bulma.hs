{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Frontend.Bulma
    ( button
    , buttonPrimary
    , card
    , cardClass
    , delete
    , hr
    , indeterminateProgress
    , tabs
    , textInput
    , title
    , title'
    , titleClass
    , titleClass'
    , subtitle
    , navItem
    , navbar
    , navMenu
    , navStart
    ) where

import Control.Lens hiding (Bifunctor, bimap, universe, element)
import Control.Lens.Indexed (iforM_, imapM)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Frontend.Prelude
import qualified Frontend.Elements as E
import Reflex.Dom.Core hiding (textInput, button)

import Obelisk.Route
import Obelisk.Route.Frontend
import Data.Proxy

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
title level s = title' level (text s)

title' :: (DomBuilder t m) => Int -> m a -> m a
title' = titleClass' ""

titleClass :: (DomBuilder t m) => Text -> Int -> Text -> m ()
titleClass cl level s = titleClass' cl level (text s)

titleClass' :: (DomBuilder t m) => Text -> Int -> m a -> m a
titleClass' cl level inner = elClass ("h" <> l) (apText ("title is-" <> l) cl) $ inner
    where l = Text.pack (show level)

subtitle :: (DomBuilder t m) => Int -> Text -> m ()
subtitle level s = elClass ("h" <> l) ("subtitle is-" <> l) $ text s
    where l = Text.pack (show level)

buttonPrimary :: (DomBuilder t m) => Text -> m (Event t ())
buttonPrimary = buttonClass "is-primary"

button :: (DomBuilder t m) => Text -> m (Event t ())
button = buttonClass ""

buttonClass :: (DomBuilder t m) => Text -> Text -> m (Event t ())
buttonClass cl t = do
    let btnClass = "button" `apText` cl
    (e, _) <- elClass' "button" btnClass $ text t
    return $ domEvent Click e

delete :: (DomBuilder t m) => m (Event t ())
delete = do
    (e, _) <- elClass' "button" "delete" $ return ()
    return $ domEvent Click e

hr :: (DomBuilder t m) => m ()
hr = elClass "hr" "hr" $ return ()

card :: (DomBuilder t m) => m a -> m a
card m = cardClass "" m

cardClass :: (DomBuilder t m) => Text -> m a -> m a
cardClass clName m = elClass "div" ("card" `apText` clName) m

apText :: Text -> Text -> Text
apText t "" = t
apText t s = t <> " " <> s

indeterminateProgress :: (DomBuilder t m) => m ()
indeterminateProgress = elAttr "progress" ("class" =:"progress is-small is-primary" <> "max" =: "100") $ text "10%"

navItem ::
     ( DomBuilder t m
     , RouteToUrl (R route) m
     , SetRoute t (R route) m
     )
     => Text
     -> R route
     -> m ()
navItem name route = routeLinkClass "navbar-item" route $ text name

navBrand :: ( DomBuilder t m
            )
            => m () -> m ()
navBrand linkEl = E.divC "navbar-brand" $ do
    linkEl
    navBurger

navBurger :: (DomBuilder t m) => m ()
navBurger = elAttr "a" attrs $ do
    el "span" $ return ()
    el "span" $ return ()
    el "span" $ return ()
        where attrs = "role" =: "button"
                      <> "class" =: "navbar-burger burger"
                      <> "onclick" =: "document.querySelector('.navbar-menu').classList.toggle('is-active');"

navbar :: ( DomBuilder t m)
          => m () -> m a -> m a
navbar  brand content = elClass "nav" "navbar" $ do
    navBrand brand
    navMenu $ do
        val <- navStart $ content
        navEnd $ return ()
        return val

navMenu :: (DomBuilder t m) => m a -> m a
navMenu = E.divC "navbar-menu"

navStart :: (DomBuilder t m) => m a -> m a
navStart = E.divC "navbar-start"

navEnd :: (DomBuilder t m) => m a -> m a
navEnd = E.divC "navbar-end"

routeLinkClass
  :: forall t m a route.
     ( DomBuilder t m
     , RouteToUrl (R route) m
     , SetRoute t (R route) m
     )
  => Text
  -> R route -- ^ Target route
  -> m a -- ^ Child widget
  -> m a
routeLinkClass className r w = do
  enc <- askRouteToUrl
  let cfg = (def :: ElementConfig EventResult t (DomBuilderSpace m))
        & elementConfig_eventSpec %~ addEventSpecFlags (Proxy :: Proxy (DomBuilderSpace m)) Click (\_ -> preventDefault)
        & elementConfig_initialAttributes .~ ("href" =: enc r <> "class" =: className)
  (e, a) <- element "a" cfg w
  setRoute $ r <$ domEvent Click e
  return a

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
