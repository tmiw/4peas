{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Forms.CommentForm 
    ( commentForm,
      commentText
    ) where

import Import

data NewComment = NewComment
    { commentText :: Textarea
    }
    deriving Show

commentForm :: Html -> MForm App App (FormResult NewComment, Widget)
commentForm = renderDivs $ NewComment
    <$> areq textareaField (fieldSettingsLabel MsgCommentFormField) Nothing