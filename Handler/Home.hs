{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.

import Data.Time
import System.Locale

prettyTime :: UTCTime -> String
prettyTime = formatTime defaultTimeLocale "%B %e, %Y %r"

getHomeR :: Handler RepHtml
getHomeR = do
    recipes <- runDB $ selectList [] [Desc RecipePosted] >>= mapM (\(Entity rId r) -> do
        let go oId = do
            o <- get404 oId
            return $ (oId, o)
        from <- go $ recipeOwner r
        comments <- selectList [RecipeCommentRecipe ==. rId] []
        return ((rId, r), (from, length comments))
        )
    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle "for peas (and cooking)"
        $(widgetFile "homepage")

getRecipeR :: RecipeId -> Handler RepHtml
getRecipeR recipeId = do
    (recipe, ingredients, steps, comments) <- runDB $ do
        recipe <- get404 recipeId
        ingredients <- selectList [IngredientRecipe ==. recipeId] []
        steps <- selectList [RecipeStepRecipe ==. recipeId] []
        comments <- selectList [RecipeCommentRecipe ==. recipeId] [Asc RecipeCommentPosted]
        return (recipe, ingredients, steps, comments)
    defaultLayout $ do
        setTitleI $ MsgRecipeTitle $ recipeName recipe
        $(widgetFile "recipe-entry")

--getHomeR = do
--    (formWidget, formEnctype) <- generateFormPost sampleForm
--    let submission = Nothing :: Maybe (FileInfo, Text)
--        handlerName = "getHomeR" :: Text
--    defaultLayout $ do
--        aDomId <- lift newIdent
--        setTitle "Welcome To Yesod!"
--        $(widgetFile "homepage")

--postHomeR :: Handler RepHtml
--postHomeR = do
--    ((result, formWidget), formEnctype) <- runFormPost sampleForm
--    let handlerName = "postHomeR" :: Text
--        submission = case result of
--            FormSuccess res -> Just res
--            _ -> Nothing

--    defaultLayout $ do
--        aDomId <- lift newIdent
--        setTitle "Welcome To Yesod!"
--        $(widgetFile "homepage")

--sampleForm :: Form (FileInfo, Text)
--sampleForm = renderDivs $ (,)
--    <$> fileAFormReq "Choose a file"
--    <*> areq textField "What's on the file?" Nothing
