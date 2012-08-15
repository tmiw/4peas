{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, MultiParamTypeClasses #-}
module Handler.Recipe where

import Import
import Data.Time (getCurrentTime)
import Yesod.Auth
import qualified Forms.RecipeForm as RF
import qualified Forms.CommentForm as CF

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
        
getNewRecipeR :: Handler RepHtml
getNewRecipeR = do
    authId <- requireAuthId
    (widget, enctype) <- generateFormPost $ RF.recipeForm Nothing
    defaultLayout $ do
        aDomId <- lift newIdent
        setTitleI $ MsgNewRecipePageTitle
        $(widgetFile "new-recipe")

getDeleteRecipeR :: RecipeId -> Handler RepHtml
getDeleteRecipeR rId = do
    authId <- requireAuthId
    defaultLayout $ do
        aDomId <- lift newIdent
        setTitleI $ MsgDeleteRecipePageTitle
        $(widgetFile "delete-recipe")
                
getEditRecipeR :: RecipeId -> Handler RepHtml
getEditRecipeR rId = do
    authId <- requireAuthId
    (recipe, ingredients, steps, tags) <- runDB $ do
        recipe <- get404 rId
        ingredients <- selectList [IngredientRecipe ==. rId] [] >>= mapM (\(Entity _ i) -> do
            return $ RF.NewRecipeIngredient (ingredientAmount i) (ingredientIngredientUnit i) (ingredientName i))
        steps <- selectList [RecipeStepRecipe ==. rId] [] >>= mapM(\(Entity _ t) -> do
            return $ recipeStepStep t)
        recipeTags <- selectList [RecipeTagRecipe ==. rId] [] >>= mapM (\(Entity _ t) -> return $ recipeTagTag t)
        tags <- selectList [TagId <-. recipeTags] [] >>= mapM(\(Entity _ t) -> return $ tagTag t)
        return (recipe, ingredients, steps, tags)
    (widget, enctype) <- generateFormPost $ RF.recipeForm $ Just (RF.NewRecipe (recipeName recipe) (recipeDescription recipe) ingredients steps tags)
    defaultLayout $ do
        aDomId <- lift newIdent
        setTitleI $ MsgEditRecipePageTitle
        $(widgetFile "edit-recipe")

postConfirmDeleteRecipeR :: RecipeId -> Handler RepHtml
postConfirmDeleteRecipeR rId = do
    runDB $ do
        _ <- deleteWhere [IngredientRecipe ==. rId]
        _ <- deleteWhere [RecipeStepRecipe ==. rId]
        _ <- deleteWhere [RecipeTagRecipe ==. rId]
        _ <- deleteWhere [RecipeCommentRecipe ==. rId]
        delete rId
    redirect $ HomeR
    
postEditRecipeR :: RecipeId -> Handler RepHtml
postEditRecipeR rId = do
    ((result, widget), enctype) <- runFormPost $ RF.recipeForm Nothing
    authId <- requireAuthId
    case result of
        FormSuccess recipe -> do
            _ <- runDB $ do
                update rId [RecipeName =. (RF.recipeName recipe), RecipeDescription =. (RF.recipeDescription recipe)]
                deleteWhere [IngredientRecipe ==. rId]
                deleteWhere [RecipeStepRecipe ==. rId]
                deleteWhere [RecipeTagRecipe ==. rId]
                _ <- insertAllIngredients rId $ RF.recipeIngredients recipe
                _ <- insertAllSteps rId $ RF.recipeSteps recipe
                insertAllTags rId $ RF.recipeTags recipe
            redirect $ RecipeR rId
        _ ->
            defaultLayout $ do
                aDomId <- lift newIdent
                setTitleI $ MsgEditRecipePageTitle
                $(widgetFile "edit-recipe")
                    
postNewRecipeR :: Handler RepHtml
postNewRecipeR = do
    ((result, widget), enctype) <- runFormPost $ RF.recipeForm Nothing
    authId <- requireAuthId
    curTime <- liftIO getCurrentTime
    case result of
        FormSuccess recipe -> do
            rId <- runDB $ do
                r <- insert $ Recipe authId curTime (RF.recipeName recipe) (RF.recipeDescription recipe)
                _ <- insertAllIngredients r $ RF.recipeIngredients recipe
                _ <- insertAllSteps r $ RF.recipeSteps recipe
                _ <- insertAllTags r $ RF.recipeTags recipe
                return r
            redirect $ RecipeR rId
        _ ->
            defaultLayout $ do
                aDomId <- lift newIdent
                setTitleI $ MsgNewRecipePageTitle
                $(widgetFile "new-recipe")

-- Utility functions for edit/add. Type definitions are to quiet warnings
-- and come from ghc itself.
insertAllSteps :: PersistStore b m => RecipeId -> [Text] -> b m [a]
insertAllSteps _ [] = return []
insertAllSteps rId (x:xs) = do
    _ <- insert $ RecipeStep rId x
    insertAllSteps rId xs
    
insertAllIngredients :: PersistStore b m => RecipeId -> [RF.NewRecipeIngredient] -> b m [a]
insertAllIngredients _ [] = return []
insertAllIngredients rId (x:xs) = do
    _ <- insert $ Ingredient rId (RF.ingredientFieldAmount x) (RF.ingredientFieldUnit x) (RF.ingredientFieldDescription x)
    insertAllIngredients rId xs

insertAllTags :: PersistQuery backend m => Key backend (RecipeGeneric backend) -> [Text] -> backend m [a]
insertAllTags _ [] = return []
insertAllTags rId (x:xs) = do
    tagId <- findOrInsertTag x
    _ <- insert $ RecipeTag rId tagId
    insertAllTags rId xs

findOrInsertTag :: PersistQuery backend m => Text -> backend m (Key backend (TagGeneric backend))
findOrInsertTag tag = do
    maybeTag <- selectFirst [TagTag ==. tag] []
    case maybeTag of
        Just (Entity eId _) -> return eId
        Nothing ->
            insert $ Tag tag