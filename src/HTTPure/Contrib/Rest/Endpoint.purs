module HTTPure.Contrib.Rest.Endpoint
  ( Endpoint
  , endpoint
  , list
  , create
  , create'
  , read
  , update
  , update'
  , delete
  , run
  ) where

import Prelude

import Control.Monad.Except as Except
import Data.Array as Array
import Data.Either as Either
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe(Just, Nothing))
import Effect.Aff (Aff)
import Foreign (ForeignError, renderForeignError)
import Foreign.Class (class Decode, class Encode, encode)
import Foreign.Generic (decodeJSON)
import Foreign.Object as Object

import HTTPure as HTTPure
import HTTPure.Contrib.Rest.Request as Request

newtype JSONErrors = JSONErrors (NonEmptyList ForeignError)

instance encodeJSONErrors :: Encode JSONErrors where
  encode (JSONErrors errors) =
    let arr = Array.fromFoldable (map renderForeignError errors)
    in encode (Object.singleton "errors" arr)

type Endpoint id =
  { list :: HTTPure.Request -> Aff HTTPure.Response
  , create :: HTTPure.Request -> Aff HTTPure.Response
  , read :: id -> HTTPure.Request -> Aff HTTPure.Response
  , update :: id -> HTTPure.Request -> Aff HTTPure.Response
  , delete :: id -> HTTPure.Request -> Aff HTTPure.Response
  , readId :: String -> Maybe id
  }

endpoint :: forall id. (String -> Maybe id) -> Endpoint id
endpoint readId =
  { list: collectionHandler
  , create: collectionHandler
  , read: instanceHandler
  , update: instanceHandler
  , delete: instanceHandler
  , readId
  }
  where
    collectionHandler _ = HTTPure.notFound
    instanceHandler _ _  = HTTPure.notFound

list
  :: forall id
   . (Request.Request Unit Unit -> Aff HTTPure.Response)
  -> Endpoint id
  -> Endpoint id
list listHandler handlers =
  handlers { list = handler }
  where
    handler httpureRequest =
      case Request.fromRequest (const $ Just unit) unit httpureRequest of
        Nothing -> HTTPure.badRequest "TODO"  -- TODO
        Just request -> listHandler request

create
  :: forall id requestBody
   . Decode requestBody
  => (Request.Request Unit requestBody -> Aff HTTPure.Response)
  -> Endpoint id
  -> Endpoint id
create =
  create' (Either.hush <<< Except.runExcept <<< decodeJSON)

create'
  :: forall id requestBody
   . (String -> Maybe requestBody)
  -> (Request.Request Unit requestBody -> Aff HTTPure.Response)
  -> Endpoint id
  -> Endpoint id
create' bodyDecoder createHandler handlers =
  handlers { create = handler }
  where
    handler httpureRequest =
      case Request.fromRequest bodyDecoder unit httpureRequest of
        Nothing -> HTTPure.badRequest "TODO" -- TODO
        Just request -> createHandler request

read
  :: forall id
   . (Request.Request id Unit -> Aff HTTPure.Response)
  -> Endpoint id
  -> Endpoint id
read readHandler handlers =
  handlers { read = handler }
  where
    handler id httpureRequest =
      case Request.fromRequest (const $ Just unit) id httpureRequest of
        Nothing -> HTTPure.badRequest "TODO"  -- TODO
        Just request -> readHandler request

update
  :: forall id requestBody
   . Decode requestBody
  => (Request.Request id requestBody -> Aff HTTPure.Response)
  -> Endpoint id
  -> Endpoint id
update =
  update' (Either.hush <<< Except.runExcept <<< decodeJSON)

update'
  :: forall id requestBody
   . (String -> Maybe requestBody)
  -> (Request.Request id requestBody -> Aff HTTPure.Response)
  -> Endpoint id
  -> Endpoint id
update' bodyDecoder updateHandler handlers =
  handlers { update = handler }
  where
    handler id httpureRequest@{ body } =
      case Request.fromRequest bodyDecoder id httpureRequest of
        Nothing -> HTTPure.badRequest "TODO" -- TODO
        Just request -> updateHandler request

delete
  :: forall id
   . (Request.Request id Unit -> Aff HTTPure.Response)
  -> Endpoint id
  -> Endpoint id
delete deleteHandler handlers =
  handlers { delete = handler }
  where
    handler id httpureRequest =
      case Request.fromRequest (const $ Just unit) id httpureRequest of
        Nothing -> HTTPure.badRequest "TODO"  -- TODO
        Just request -> deleteHandler request

run
  :: forall id
   . Endpoint id
  -> HTTPure.Request
  -> Aff HTTPure.Response
run ep request@{ path, method } =
  handle method path
  where
    handle HTTPure.Get [] = ep.list request
    handle HTTPure.Post [] = ep.create request
    handle HTTPure.Get [id] = withId id ep.read
    handle HTTPure.Put [id] = withId id ep.update
    handle HTTPure.Delete [id] = withId id ep.delete
    handle _ _ = HTTPure.notFound

    withId idString handler =
      case ep.readId idString of
        Just id -> handler id request
        Nothing -> HTTPure.notFound
