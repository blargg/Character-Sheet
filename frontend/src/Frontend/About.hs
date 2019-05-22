{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Frontend.About where

import Reflex.Dom
import Obelisk.Route.Frontend
import Common.Route
import qualified Frontend.Elements as EL

main :: (DomBuilder t m
             , SetRoute t (R FrontendRoute) m
             , RouteToUrl (R FrontendRoute) m
              )
             => m ()
main = do
    el "div" $ routeLink (FrontendRoute_Main :/ ()) $ text "sheet"
    el "h1" $ text "About"
    el "p" $ text "GitHub: " >> EL.urlLink projectURL "project page"
    el "h2" $ text "Caution"
    el "p" $ text "This website is experimental, and provided as is. \
        \This is not meant to hold onto a character sheet long term, \
        \and data might get lost over time. \
        \(As of writing, data is not saved after you leave the page.) \
        \Please copy down anything that you will need later."
    el "h2" $ text "Helping Out"
    el "p" $ do
        text "If you like this project, consider contributing "
        EL.urlLink projectURL "code changes"
        text ", "
        EL.urlLink issuesURL "suggestions"
        text ", or "
        EL.urlLink issuesURL "bug reports"
        text " on the project page."
        where projectURL = "https://github.com/blargg/Character-Sheet"
              issuesURL = projectURL <> "/issues"

