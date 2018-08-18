module HTTPure.Contrib.Rest.Request (Request, fromRequest) where

import Prelude

import Data.Maybe (Maybe)
import HTTPure as HTTPure

type Request id body =
  { method :: HTTPure.Method
  , path :: HTTPure.Path
  , query :: HTTPure.Query
  , headers :: HTTPure.Headers
  , id :: id
  , body :: body
  }

fromRequest
  :: forall id body
   . (String -> Maybe body)
  -> id
  -> HTTPure.Request
  -> Maybe (Request id body)
fromRequest readBody id { method, path, query, headers, body } = do
  body' <- readBody body
  pure $ { method, path, query, headers, id, body: body' }
