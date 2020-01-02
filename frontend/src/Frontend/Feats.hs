{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Frontend.Feats (main) where

import Reflex.Dom

import Common.Api
import Data.CharacterSheet
import Data.Text (Text)
import Frontend.Input
import Frontend.Layout
import Frontend.Prelude
import qualified Frontend.Bulma as Bulma
import qualified Frontend.Elements as E

main :: AppWidget t m => m ()
main = Bulma.cardClass "page top-margin" $ do
    featSearch

featSearch :: forall t m. AppWidget t m => m ()
featSearch = mdo
    query <- searchBox
    pb <- getPostBuild
    let search = PagedSearch <$> query <*> curPage
    let initialLoad = tag (current search) pb
    let featLoadReqEvents = fmap featRequest $ leftmost [ updated search, initialLoad ]
    featLoad <- fmapMaybe decodeXhrResponse <$> performRequestAsync featLoadReqEvents :: m (Event t FeatSearchResponse)
    feats <- holdDyn [] (fmap pageData featLoad)
    numberOfPages <- holdDyn 1 (fmap totalPages featLoad)
    displayFeatList feats
    curPage <- Bulma.pagination (() <$ updated query) numberOfPages
    return ()

featRequest :: FeatSearch -> XhrRequest Text
featRequest s = postJson "api/featlist" s

searchBox :: AppWidget t m => m (Dynamic t FeatQuery)
searchBox = E.divC "control" $ do
    namePrefix <- E.divC "field" $ Bulma.textInput "Search"
    return $ FeatQuery <$> namePrefix

displayFeatList :: AppWidget t m => Dynamic t [Feat] -> m ()
displayFeatList feats = do
    _ <- dyn $ fmap (mapM_ displayFeat') feats
    return ()
        where displayFeat' feat = displayFeat feat (return ())

displayFeat :: AppWidget t m => Feat -> m a -> m a
displayFeat Feat{..} rightLevel = do
    (expanded, result) <- Bulma.level $ do
        left <- Bulma.levelLeft $ do
            Bulma.levelItem $ Bulma.titleClass "is-marginless" 5 featName
            Bulma.levelItem $ expandCollapseText Closed
        right <- Bulma.levelRight $ rightLevel
        return (left, right)
    collapseSection expanded $ do
        E.div . text $ featDescription
        E.div $ lbl' "Type" >> space' >> E.span (text . fmt $ featType)
        E.div $ lbl' "Prerequesits" >> space' >> E.span (text featPrereqs)
        E.div $ lbl' "Effect" >> space' >> E.span (text featEffect)
        E.div $ lbl' "Normal" >> space' >> E.span (text normalEffect)
        E.div $ lbl' "Extra Information" >> space' >> E.span (text featExtra)
    return result
