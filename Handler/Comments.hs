{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Comments where

import Import
import Data.Time (getCurrentTime)
import Yesod.Auth

data NewComment = NewComment
    { commentText :: Textarea
    }
    deriving Show

commentForm :: Html -> MForm App App (FormResult NewComment, Widget)
commentForm = renderDivs $ NewComment
    <$> areq textareaField "Comment" Nothing

getCommentR :: RecipeId -> Handler RepHtml
getCommentR recipe = do
    -- This handles the case where the user isn't logged in when posting the comment.
    redirect $ RecipeR recipe

postCommentR :: RecipeId -> Handler RepHtml
postCommentR recipe = do
    ((result, widget), enctype) <- runFormPost commentForm
    curTime <- liftIO getCurrentTime
    authId <- requireAuthId
    case result of
        FormSuccess comment -> do
            _ <- runDB $ insert $ RecipeComment recipe authId curTime (commentText comment)
            redirect $ RecipeR recipe
        _ ->
            undefined -- TODO