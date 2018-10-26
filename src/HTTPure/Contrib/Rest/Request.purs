module HTTPure.Contrib.Rest.Request (Request, fromRequest) where

import Prelude

import Data.Either (Either)
import Foreign as Foreign
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
   . (String -> Either Foreign.MultipleErrors body)
  -> id
  -> HTTPure.Request
  -> Either Foreign.MultipleErrors (Request id body)
fromRequest readBody id { method, path, query, headers, body } = do
  body' <- readBody body
  pure $ { method, path, query, headers, id, body: body' }
