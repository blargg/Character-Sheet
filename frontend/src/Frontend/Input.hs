{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Frontend.Input
    ( Closed(..)
    , NumberConfig(..)
    , collapseSection
    , buttonC
    , editSpan
    , input_checkbox
    , listWidget
    , listWidget'
    , multiLineWidget
    , noValue
    , numberInput
    , numberInput'
    , percentageInput
    , expandCollapseButton
    , expandCollapseText
    , textSimple
    , simpleConfig
    ) where

import Common.Compose
import Control.Lens
import Control.Monad.Fix
import Data.CharacterSheet (Percentage(..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

import Language.Javascript.JSaddle
import Reflex.Dom.Core
import Obelisk.Generated.Static

import Frontend.Prelude
import Frontend.Javascript
import Frontend.Bulma as Bulma
import Frontend.Layout (row)

-- Editable percentage. Allows for percentages over 100%
percentageInput :: (DomBuilder t m) => Percentage -> m (Dynamic t (Maybe Percentage))
percentageInput (Percentage x) = fmap Percentage <$$> numberInput x

numberInput :: ( Read a
               , Show a
               , DomBuilder t m
               )
               => a -> m (Dynamic t (Maybe a))
numberInput = numberInput' . fromInitalValue

fromInitalValue :: a -> NumberConfig a
fromInitalValue x = NumberConfig { numberVal = Just x
                                 , cssClass = "numberInput num"
                                 }

simpleConfig :: Maybe a -> NumberConfig a
simpleConfig x = NumberConfig { numberVal = x
                              , cssClass = "numberInput num"
                              }

noValue :: NumberConfig a
noValue = NumberConfig { numberVal = Nothing
                       , cssClass = "numberInput num"
                       }

data NumberConfig a = NumberConfig { numberVal :: Maybe a
                                   , cssClass :: Text
                                   }

-- like number input, but allows empty initial value
numberInput' :: ( Read a
               , Show a
               , DomBuilder t m
               )
               => NumberConfig a -> m (Dynamic t (Maybe a))
numberInput' conf = parseInput parse numberConfig
    where
        initialVal = numberVal conf
        cl = cssClass conf
        numberConfig = def & elConf .~ (  classAttr cl
                                       <> "type" =: "number")
                           & inputElementConfig_initialValue
                             .~ (T.pack . maybe "" show $ initialVal)
        elConf = inputElementConfig_elementConfig . elementConfig_initialAttributes
        parse :: (Read a) => Text -> Maybe a
        parse = readMaybe . T.unpack


-- TODO set a class depending on valid or invalid
parseInput :: (DomBuilder t m)
           => (T.Text -> Maybe a) -> InputElementConfig er t (DomBuilderSpace m) -> m (Dynamic t (Maybe a))
parseInput parse config = do
    textValue <- inputElement config
    return $ fmap parse $ value textValue

textSimple :: (DomBuilder t m)
          => T.Text
          -> m (Dynamic t T.Text)
textSimple initVal = do
    let config = def & inputElementConfig_initialValue .~ initVal
    textValue <- inputElement config
    return $ value textValue

-- A span that a user can edit in place (using contenteditable = true)
editSpan :: ( DomBuilder t m
            , MonadHold t m
            , PerformEvent t m
            , MonadJSM (Performable m)
            ) => Text -> Text -> m (Dynamic t Text)
editSpan elemId initialText = do
    (e, ()) <- elAttr' "span" editAttrs $ text initialText
    let changedEv = domEvent Input e
    let checkUpdate = fmap (fromMaybe "") . liftJSM $ innerTextById elemId
    let checkUpdateEv = checkUpdate <$ changedEv
    updatedValues <- performEvent checkUpdateEv
    holdDyn initialText updatedValues
    where editAttrs = "class" =: "single-line-edit"
                    <> "contenteditable" =: "true"
                    <> "id" =: elemId

innerTextById :: Text -> JSM (Maybe Text)
innerTextById elemId = do
    innerHTML <- jsg ("document" :: Text)
        ^. js1 ("getElementById" :: Text) elemId
        ^. js ("innerText" :: Text)
    jsvToText innerHTML

data Closed = Closed | Open
    deriving (Eq)

isOpen :: Bool -> Closed
isOpen True = Open
isOpen False = Closed

-- Creates a button that toggles, and changes image when clicked.
-- The appearance is suited for expand and collapse.
expandCollapseButton :: forall t m. (DomBuilder t m, MonadFix m, MonadHold t m, PostBuild t m) => Closed -> m (Dynamic t Closed)
expandCollapseButton initState = mdo
    let initiallyOpen = initState == Open
    expState <- isOpen <$$> toggle initiallyOpen clickedEv :: m (Dynamic t Closed)
    (openSvgEl, _) <- mkSvg Open expState
    (closeSvgEl, _) <- mkSvg Closed expState
    let clickedEv :: Event t ()
        clickedEv = domEvent Click openSvgEl <> domEvent Click closeSvgEl
    return expState
    where
        mkSvg rep cur = elDynAttr'
            "img"
            (
                (displayStyle rep <$> cur)
                <> pure ("src" =: iconFor rep
                      <> "height" =: size
                      <> "width" =: size))
            (return ())
        iconFor Open = static @"icon/up_arrow.svg"
        iconFor Closed = static @"icon/down_arrow.svg"
        displayStyle rep cur = if rep == cur then mempty
                                             else "style" =: "display: none;"
        size = "10px"

expandCollapseText :: forall t m. (DomBuilder t m, MonadFix m, MonadHold t m, PostBuild t m) => Closed -> m (Dynamic t Closed)
expandCollapseText initState = mdo
    let initiallyOpen = initState == Open
    expState <- isOpen <$$> toggle initiallyOpen clickedEv :: m (Dynamic t Closed)
    openEv <- elDynAttr "div" (displayStyle Open <$> expState) $ widget Open
    closeEv <- elDynAttr "div" (displayStyle Closed <$> expState) $ widget Closed
    let clickedEv :: Event t ()
        clickedEv = closeEv <> openEv
    return expState
    where
        widget Open = do
            (l, _) <- elClass' "a" "extra" (text "less")
            return $ domEvent Click l
        widget Closed = do
            (l, _) <- elClass' "a" "extra" (text "more")
            return $ domEvent Click l
        displayStyle rep cur = if rep == cur then mempty
                                             else "style" =: "display: none;"

buttonC :: DomBuilder t m => Text -> Text -> m (Event t ())
buttonC className buttonText = do
    (e, ()) <- elClass' "button" className $ text buttonText
    return $ domEvent Click e

collapseSection :: ( DomBuilder t m
                   , PostBuild t m
                   ) => Dynamic t Closed -> m a -> m a
collapseSection isClosed = elDynAttr "div" dynAttrs
    where attrs Closed = "style" =: "display: none"
          attrs Open = mempty
          dynAttrs = fmap attrs isClosed

listWidget :: (AppWidget t m, Monoid b)
           => [a]
           -> a
           -> (a -> m b)
           -> m (Dynamic t b)
listWidget initList newLine mkWidget = listWidget' initList newLine mkWidgetWithDelete
    where mkWidgetWithDelete x = row $ do
              v <- mkWidget x
              d <- Bulma.delete
              return (v, d)

listWidget' :: (AppWidget t m, Monoid b)
           => [a]
           -> a
           -> (a -> m (b, Event t ()))
           -> m (Dynamic t b)
listWidget' initList newLine mkWidget = do
    dynMap <- multiLineWidget (mkMap initList) newLine mkWidget
    return . fmap mconcat $ snd <$$> Map.toList <$> dynMap
        where enumerate = zip [0..]
              mkMap :: [a] -> Map Int a
              mkMap = Map.fromList . enumerate

multiLineWidget :: forall t m a b. (AppWidget t m)
                => Map Int a
                -> a
                -> (a -> m (b, (Event t ())))
                -> m (Dynamic t (Map Int b))
multiLineWidget initVal newLine lineWidget = mdo
    lineDyn <- listWithKeyShallowDiff initVal updates (keyedRow lineWidget)
        :: m (Dynamic t (Map Int (b, Event t Int)))
    addPressed <- el "div" $ Bulma.buttonPrimary "New" :: m (Event t ())
    let addLines :: Event t (Map Int (Maybe a))
        addLines = attachWith (\m _ -> nextKey m =: Just newLine) (current lineDyn) addPressed
    let removeLines :: Event t (Map Int (Maybe a))
        removeLines = (\k -> k =: Nothing) <$> removeEvents lineDyn
    let updates :: Event t (Map Int (Maybe a))
        updates = (addLines <> removeLines)
    return $ (fmap . fmap) fst $ lineDyn
    where removeEvents :: forall t' b'. (Reflex t')
                       => Dynamic t' (Map Int (b', Event t' Int))
                       -> (Event t' Int)
          removeEvents x = switchPromptlyDyn $ leftmost . fmap (snd . snd) . Map.toList <$> x

keyedRow :: (AppWidget t m)
         => (a -> m (b, Event t ())) -> Int -> a -> Event t a -> m (b, Event t Int)
keyedRow mkWidget key initVal _ = do
    (x, remove) <- mkWidget initVal
    return (x, key <$ remove)

nextKey :: (Num k) => Map k a -> k
nextKey = (1+) . fromMaybe 0 . maxKey

maxKey :: Map k a -> Maybe k
maxKey = fmap fst . lookupMax

lookupMax :: Map k a -> Maybe (k, a)
lookupMax m | Map.null m = Nothing
            | otherwise = Just (Map.findMax m)

input_checkbox :: (DomBuilder t m) => Bool -> m (Dynamic t Bool)
input_checkbox checked = do
  let permanentAttrs = "type" =: "checkbox"
  i <- inputElement $ def
    & inputElementConfig_initialChecked .~ checked
    & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ Map.mapKeys (AttributeName Nothing) permanentAttrs
  return $ _inputElement_checked i
