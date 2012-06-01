{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, MultiParamTypeClasses #-}
module Handler.NewRecipe where

import Import
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Yesod.Auth

getNewRecipeR :: Handler RepHtml
getNewRecipeR = do
    (widget, enctype) <- generateFormPost recipeForm
    defaultLayout $ do
        aDomId <- lift newIdent
        setTitleI $ MsgNewRecipePageTitle
        $(widgetFile "new-recipe")
        
postNewRecipeR :: Handler RepHtml
postNewRecipeR = do
    ((result, widget), enctype) <- runFormPost recipeForm
    authId <- requireAuthId
    curTime <- liftIO getCurrentTime
    case result of
        FormSuccess recipe -> do
            rId <- runDB $ insert $ Recipe authId curTime (Handler.NewRecipe.recipeName recipe) (Handler.NewRecipe.recipeDescription recipe)
            _ <- runDB $ insertAllSteps rId $ recipeSteps recipe
            _ <- runDB $ insertAllTags rId $ recipeTags recipe
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

validateTextList :: [Text] -> GHandler sub master (Either (SomeMessage master) (Maybe [Text]))
validateTextList rawVals =
    if any lengthNonZero rawVals then
        return $ Right $ Just $ filterVals rawVals
    else
        if length rawVals > 0 then
            return $ Left $ "error"
        else
            return $ Right $ Nothing
    where 
        filterVals vals = filter lengthNonZero vals
        lengthNonZero v = (T.length v) > 0

recipeStepsField :: Field sub master [Text]
recipeStepsField = Field
    { fieldParse = validateTextList
    , fieldView = \idAttr nameAttr _ eResult _ -> [whamlet|
<ol id=#{idAttr} class="recipeSteps">
    $case eResult
        $of Left errVal
            <li>#{errVal}
        $of Right listVal
            $forall val <- listVal
                <li>
                    <input name=#{nameAttr} type="text" value=#{val}>
<input type="button" name=#{idAttr}-add value="xyz" onClick="addStep('#{idAttr}', '#{nameAttr}')";>
|]
    }

recipeTagsField :: Field sub master [Text]
recipeTagsField = Field
    { fieldParse = validateTextList
    , fieldView = \idAttr nameAttr _ eResult _ -> [whamlet|
<ol id=#{idAttr} class="recipeTags">
    $case eResult
        $of Left errVal
            <li>#{errVal}
        $of Right listVal
            $forall val <- listVal
                <li>
                    <input name=#{nameAttr} type="text" value=#{val}>
<input type="button" name=#{idAttr}-add value="xyz" onClick="addTag('#{idAttr}', '#{nameAttr}')";>
|]
    }
        
data NewRecipe = NewRecipe
    { recipeName :: Text
    , recipeDescription :: Textarea
    , recipeSteps :: [Text]
    , recipeTags :: [Text]
    }
    deriving Show

recipeForm :: Html -> MForm App App (FormResult NewRecipe, Widget)
recipeForm = renderDivs $ NewRecipe
    <$> areq textField "Name" Nothing
    <*> areq textareaField "Description" Nothing
    <*> areq recipeStepsField "Steps" Nothing
    <*> areq recipeTagsField "Tags" Nothing