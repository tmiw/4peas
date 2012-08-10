{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import qualified Forms.CommentForm as CF

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.

data RecipeUIEntry = RecipeUIEntry (RecipeId, Recipe) (UserId, User) Int [Entity Tag]

resultsPerPage :: Int
resultsPerPage = 10

getResults :: Int -> [Filter Recipe] -> Handler [RecipeUIEntry]
getResults pageNumber conditions = do
    runDB $ selectList conditions [Desc RecipePosted, LimitTo resultsPerPage, OffsetBy $ (pageNumber - 1) * resultsPerPage] >>= mapM (\(Entity rId r) -> do
        let go oId = do
            o <- get404 oId
            return $ (oId, o)
        from <- go $ recipeOwner r
        commentCount <- count [RecipeCommentRecipe ==. rId]
        recipeTags <- selectList [RecipeTagRecipe ==. rId] [] >>= mapM (\(Entity _ t) -> return $ recipeTagTag t)
        tags <- selectList [TagId <-. recipeTags] []
        return $ (RecipeUIEntry (rId, r) from commentCount tags)
        )

getHomePageR :: Int -> Handler RepHtml
getHomePageR pageNumber = do
    recipes <- getResults pageNumber []
    defaultLayout $ do
        aDomId <- lift newIdent
        setTitleI $ MsgHomePageTitle
        $(widgetFile "homepage")
    where
        isNotFirstPage pn = if (pn > 1) then True else False
        isNotLastPage r = if ((length r) < resultsPerPage) then False else True
        nextPageNumber = pageNumber + 1
        prevPageNumber = pageNumber - 1

getHomeTagPageR :: Text -> Int -> Handler RepHtml
getHomeTagPageR tag pageNumber = do
    tagId <- runDB $ do
        tmpTag <- selectFirst [TagTag ==. tag] []
        return $ getTagId tmpTag
    recipeTags <- runDB $ selectList [RecipeTagTag ==. tagId] [] >>= mapM (\(Entity _ rt) -> return $ recipeTagRecipe rt)
    recipes <- getResults pageNumber [RecipeId <-. recipeTags]
    defaultLayout $ do
        aDomId <- lift newIdent
        setTitleI $ MsgHomePageTitle
        $(widgetFile "homepage")
    where
        isNotFirstPage pn = if (pn > 1) then True else False
        isNotLastPage r = if ((length r) < resultsPerPage) then False else True
        nextPageNumber = pageNumber + 1
        prevPageNumber = pageNumber - 1
        getTagId (Just (Entity tId _)) = tId
        getTagId Nothing = error "Invalid tag provided."

getHomeTagR :: Text -> Handler RepHtml
getHomeTagR tag = getHomeTagPageR tag 1

getHomeR :: Handler RepHtml
getHomeR = getHomePageR 1

getRecipeR :: RecipeId -> Handler RepHtml
getRecipeR recipeId = do
    (recipe, from, ingredients, steps, comments, commentCount, tags) <- runDB $ do
        recipe <- get404 recipeId
        from <- get404 $ recipeOwner recipe
        ingredients <- selectList [IngredientRecipe ==. recipeId] [] >>= mapM (\(Entity _ i) -> do
            unit <- case (ingredientIngredientUnit i) of
                Nothing -> return Nothing
                Just u -> get $ u
            return (ingredientAmount i, unit, ingredientName i))
        steps <- selectList [RecipeStepRecipe ==. recipeId] []
        comments <- selectList [RecipeCommentRecipe ==. recipeId] [Asc RecipeCommentPosted]
        commentCount <- count [RecipeCommentRecipe ==. recipeId]
        recipeTags <- selectList [RecipeTagRecipe ==. recipeId] [] >>= mapM (\(Entity _ t) -> return $ recipeTagTag t)
        tags <- selectList [TagId <-. recipeTags] []
        return (recipe, from, ingredients, steps, comments, commentCount, tags)
    (widget, enctype) <- generateFormPost CF.commentForm
    authId <- maybeAuthId
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
