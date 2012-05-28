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

validateRecipeSteps :: [Text] -> GHandler sub master (Either (SomeMessage master) (Maybe [Text]))
validateRecipeSteps rawVals =
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
    { fieldParse = validateRecipeSteps
    , fieldView = \idAttr nameAttr _ eResult _ -> [whamlet|
<ol id=#{idAttr}>
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

data NewRecipe = NewRecipe
    { recipeName :: Text
    , recipeDescription :: Textarea
    , recipeSteps :: [Text]
    }
    deriving Show

recipeForm :: Html -> MForm App App (FormResult NewRecipe, Widget)
recipeForm = renderDivs $ NewRecipe
    <$> areq textField "Name" Nothing
    <*> areq textareaField "Description" Nothing
    <*> areq recipeStepsField "Steps" Nothing