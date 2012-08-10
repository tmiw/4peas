{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Comments where

import Import
import Data.Time (getCurrentTime)
import Yesod.Auth
import qualified Forms.CommentForm as F

getCommentR :: RecipeId -> Handler RepHtml
getCommentR recipe = do
    -- This handles the case where the user isn't logged in when posting the comment.
    redirect $ RecipeR recipe

postCommentR :: RecipeId -> Handler RepHtml
postCommentR recipe = do
    ((result, widget), enctype) <- runFormPost F.commentForm
    curTime <- liftIO getCurrentTime
    authId <- requireAuthId
    case result of
        FormSuccess comment -> do
            _ <- runDB $ insert $ RecipeComment recipe authId curTime (F.commentText comment)
            redirect $ RecipeR recipe
        _ ->
            undefined -- TODO