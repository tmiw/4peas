{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, MultiParamTypeClasses #-}
module Forms.RecipeForm
    ( recipeForm,
      NewRecipeIngredient(NewRecipeIngredient),
      NewRecipe(NewRecipe),
      Forms.RecipeForm.recipeName,
      Forms.RecipeForm.recipeDescription,
      recipeIngredients,
      recipeSteps,
      recipeTags,
      ingredientFieldAmount,
      ingredientFieldUnit,
      ingredientFieldDescription
    ) where

import Import
import qualified Data.Text as T
import Data.Text.Read (double)
import Database.Persist.Store
import Data.Int (Int64)

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

maybeCallFunc :: Maybe a -> (a -> b) -> Maybe b
maybeCallFunc Nothing _ = Nothing
maybeCallFunc (Just obj) fn = Just (fn obj)

recipeForm :: Maybe NewRecipe -> Html -> MForm App App (FormResult NewRecipe, Widget)
recipeForm r = renderDivs $ NewRecipe
    <$> areq textField (fieldSettingsLabel MsgRecipeNameFormField) (recipeNameFieldValue r)
    <*> areq textareaField (fieldSettingsLabel MsgRecipeDescriptionFormField) (recipeDescriptionFieldValue r)
    <*> areq recipeIngredientsField (fieldSettingsLabel MsgRecipeIngredientsFormField) (recipeIngredientsFieldValue r)
    <*> areq recipeStepsField (fieldSettingsLabel MsgRecipeStepsFormField) (recipeStepsFieldValue r)
    <*> areq recipeTagsField (fieldSettingsLabel MsgRecipeTagsFormField) (recipeTagsFieldValue r)
    where
        recipeNameFieldValue recipe = maybeCallFunc recipe Forms.RecipeForm.recipeName
        recipeDescriptionFieldValue recipe = maybeCallFunc recipe Forms.RecipeForm.recipeDescription
        recipeIngredientsFieldValue recipe = maybeCallFunc recipe Forms.RecipeForm.recipeIngredients
        recipeStepsFieldValue recipe = maybeCallFunc recipe Forms.RecipeForm.recipeSteps
        recipeTagsFieldValue recipe = maybeCallFunc recipe Forms.RecipeForm.recipeTags