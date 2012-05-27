{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.NewRecipe where

import Import

getNewRecipeR :: Handler RepHtml
getNewRecipeR = 
    defaultLayout $ do
        aDomId <- lift newIdent
        setTitleI $ MsgNewRecipePageTitle
        $(widgetFile "new-recipe")
        
postNewRecipeR :: Handler RepHtml
postNewRecipeR = 
    defaultLayout $ do
        aDomId <- lift newIdent
        setTitleI $ MsgNewRecipePageTitle
        $(widgetFile "new-recipe")