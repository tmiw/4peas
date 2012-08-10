{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, MultiParamTypeClasses #-}
module Handler.NewRecipe where

import Import
import qualified Data.Text as T
import Data.Text.Read (double)
import Data.Time (getCurrentTime)
import Database.Persist.Store
import Data.Int (Int64)
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
            _ <- runDB $ insertAllIngredients rId $ recipeIngredients recipe
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
        insertAllIngredients _ [] = return []
        insertAllIngredients rId (x:xs) = do
            _ <- insert $ Ingredient rId (ingredientFieldAmount x) (ingredientFieldUnit x) (ingredientFieldDescription x)
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

recipeStepsField :: (RenderMessage master AppMessage) => Field sub master [Text]
recipeStepsField = Field
    { fieldParse = validateTextList
    , fieldView = \idAttr nameAttr _ eResult _ -> [whamlet|
<div class="dyn_list">
    <ol id=#{idAttr} class="recipeSteps">
        $case eResult
            $of Left errVal
                $if T.null errVal
                
                $else
                    <li>#{errVal}
            $of Right listVal
                $forall val <- listVal
                    <li>
                        <input name=#{nameAttr} type="text" value=#{val}>
    <input type="button" name=#{idAttr}-add value=_{MsgAddStepButton} onClick="addStep('#{idAttr}', '#{nameAttr}')";>
|]
    }

recipeTagsField :: (RenderMessage master AppMessage) => Field sub master [Text]
recipeTagsField = Field
    { fieldParse = validateTextList
    , fieldView = \idAttr nameAttr _ eResult _ -> [whamlet|
<div class="dyn_list">
    <ol id=#{idAttr} class="recipeTags">
        $case eResult
            $of Left errVal
                $if T.null errVal
                
                $else
                    <li>#{errVal}
            $of Right listVal
                $forall val <- listVal
                    <li>
                        <input name=#{nameAttr} type="text" value=#{val}>
    <input type="button" name=#{idAttr}-add value=_{MsgAddTagButton} onClick="addTag('#{idAttr}', '#{nameAttr}')";>
|]
    }

validateIngredientList :: [Text] -> GHandler sub master (Either (SomeMessage master) (Maybe [NewRecipeIngredient]))
validateIngredientList rawVals =
    if all lengthNonZero rawVals then do
        return $ Right $ Just $ makeDataObject $ rawVals
    else
        if length rawVals > 0 then
            return $ Left $ "error"
        else
            return $ Right $ Nothing
    where
        lengthNonZero v = (T.length v) > 0
        makeDataObject :: [Text] -> [NewRecipeIngredient]
        makeDataObject [] = []
        makeDataObject (amount:unit:desc:xs) =
            (NewRecipeIngredient (toDoubleType amount) (makeUnitId unit) desc) : makeDataObject xs
        makeUnitId v | T.length v > 0 = Just $ toIdType v
                     | otherwise    = Nothing
        toIdType v = Key $ PersistInt64 $ (read (T.unpack v) :: Int64)
        toDoubleType v = case (double v) of
            Left errStr -> error errStr
            Right (val, _) -> val
        
recipeIngredientsField :: (RenderMessage master AppMessage) => Field sub master [NewRecipeIngredient]
recipeIngredientsField = Field
    { fieldParse = validateIngredientList
    , fieldView = \idAttr nameAttr _ eResult _ -> [whamlet|
<div class="dyn_list">
    <ol id=#{idAttr} class="recipeIngredients">
        $case eResult
            $of Left errVal
                $if T.null errVal
                    
                $else
                    <li>#{errVal}
            $of Right listVal
                $forall val <- listVal
                    <li>
                        <input class="dyn_list_element_quantity" name=#{nameAttr} type="text" value=#{ingredientFieldAmount val}>
                        <select class="dyn_list_element" name=#{nameAttr}>
                            <option value="0" selected>
                        <input class="dyn_list_element" name=#{nameAttr} type="text" value=#{ingredientFieldDescription val}>
    <input type="button" name=#{idAttr}-add value=_{MsgAddIngredientButton} onClick="addIngredient('#{idAttr}', '#{nameAttr}')";>
|]
    }

data NewRecipeIngredient = NewRecipeIngredient
    { ingredientFieldAmount :: Double
    , ingredientFieldUnit :: Maybe UnitId
    , ingredientFieldDescription :: Text
    }
    deriving Show

data NewRecipe = NewRecipe
    { recipeName :: Text
    , recipeDescription :: Textarea
    , recipeIngredients :: [NewRecipeIngredient]
    , recipeSteps :: [Text]
    , recipeTags :: [Text]
    }
    deriving Show

recipeForm :: Html -> MForm App App (FormResult NewRecipe, Widget)
recipeForm = renderDivs $ NewRecipe
    <$> areq textField (fieldSettingsLabel MsgRecipeNameFormField) Nothing
    <*> areq textareaField (fieldSettingsLabel MsgRecipeDescriptionFormField) Nothing
    <*> areq recipeIngredientsField (fieldSettingsLabel MsgRecipeIngredientsFormField) Nothing
    <*> areq recipeStepsField (fieldSettingsLabel MsgRecipeStepsFormField) Nothing
    <*> areq recipeTagsField (fieldSettingsLabel MsgRecipeTagsFormField) Nothing