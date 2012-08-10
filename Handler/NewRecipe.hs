{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, MultiParamTypeClasses #-}
module Handler.NewRecipe where

import Import
import Data.Time (getCurrentTime)
import Yesod.Auth
import qualified Forms.RecipeForm as F

getNewRecipeR :: Handler RepHtml
getNewRecipeR = do
    (widget, enctype) <- generateFormPost F.recipeForm
    defaultLayout $ do
        aDomId <- lift newIdent
        setTitleI $ MsgNewRecipePageTitle
        $(widgetFile "new-recipe")
        
postNewRecipeR :: Handler RepHtml
postNewRecipeR = do
    ((result, widget), enctype) <- runFormPost F.recipeForm
    authId <- requireAuthId
    curTime <- liftIO getCurrentTime
    case result of
        FormSuccess recipe -> do
            rId <- runDB $ insert $ Recipe authId curTime (F.recipeName recipe) (F.recipeDescription recipe)
            _ <- runDB $ insertAllIngredients rId $ F.recipeIngredients recipe
            _ <- runDB $ insertAllSteps rId $ F.recipeSteps recipe
            _ <- runDB $ insertAllTags rId $ F.recipeTags recipe
            redirect $ RecipeR rId
        _ ->
            defaultLayout $ do
                aDomId <- lift newIdent
                setTitleI $ MsgNewRecipePageTitle
                $(widgetFile "new-recipe")
    where
        insertAllSteps _ [] = return []
        insertAllSteps rId (x:xs) = do
            _ <- insert $ RecipeStep rId x
            insertAllSteps rId xs
        insertAllIngredients _ [] = return []
        insertAllIngredients rId (x:xs) = do
            _ <- insert $ Ingredient rId (F.ingredientFieldAmount x) (F.ingredientFieldUnit x) (F.ingredientFieldDescription x)
            insertAllIngredients rId xs
        insertAllTags _ [] = return []
        insertAllTags rId (x:xs) = do
            tagId <- findOrInsertTag x
            _ <- insert $ RecipeTag rId tagId
            insertAllTags rId xs
        findOrInsertTag tag = do
            maybeTag <- selectFirst [TagTag ==. tag] []
            case maybeTag of
                Just (Entity eId _) -> return eId
                Nothing ->
                    insert $ Tag tag