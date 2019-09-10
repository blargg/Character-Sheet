{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Frontend.About (main) where

import Reflex.Dom
import qualified Frontend.Elements as EL

main :: DomBuilder t m => m ()
main = do
    el "h1" $ text "About"
    el "p" $ text "GitHub: " >> EL.urlLink projectURL "project page"
    el "h2" $ text "Caution"
    el "p" $ text "This website is still experimental, and provided as is. \
        \This is will not hold onto a character sheet long term, \
        \and data will get lost when the site updates or when you clear your browsing data. \
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

