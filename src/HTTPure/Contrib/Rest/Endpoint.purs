module HTTPure.Contrib.Rest.Endpoint
  ( Endpoint
  , CollectionHandler
  , InstanceHandler
  , SubPathHandler
  , endpoint
  , list
  , create
  , create'
  , read
  , update
  , update'
  , delete
  , collectionGET
  , collectionPOST
  , collectionPOST'
  , instanceGET
  , instancePOST
  , instancePOST'
  , run
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except as Except
import Data.Array as Array
import Data.Either as Either
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Maybe as Maybe
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


type CollectionHandler = HTTPure.Request -> Aff HTTPure.Response
type InstanceHandler id = id -> HTTPure.Request -> Aff HTTPure.Response
data SubPathHandler a = SubPathHandler HTTPure.Method HTTPure.Path a

type Endpoint id =
  { list :: CollectionHandler
  , create :: CollectionHandler
  , read :: InstanceHandler id
  , update :: InstanceHandler id
  , delete :: InstanceHandler id
  , collectionHandlers :: Array (SubPathHandler CollectionHandler)
  , instanceHandlers :: Array (SubPathHandler (InstanceHandler id))
  , readId :: String -> Maybe id
  }

endpoint :: forall id. (String -> Maybe id) -> Endpoint id
endpoint readId =
  { list: collectionNotFound
  , create: collectionNotFound
  , read: instanceNotFound
  , update: instanceNotFound
  , delete: instanceNotFound
  , collectionHandlers: []
  , instanceHandlers: []
  , readId
  }
  where
    collectionNotFound _ = HTTPure.notFound
    instanceNotFound _ _ = HTTPure.notFound

list
  :: forall id
   . (Request.Request Unit Unit -> Aff HTTPure.Response)
  -> Endpoint id
  -> Endpoint id
list listHandler handlers =
  handlers { list = collectionHandler listHandler }


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
  handlers { create = collectionHandlerWithBody bodyDecoder createHandler }


read
  :: forall id
   . (Request.Request id Unit -> Aff HTTPure.Response)
  -> Endpoint id
  -> Endpoint id
read readHandler handlers =
  handlers { read = instanceHandler readHandler }


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
  handlers { update = instanceHandlerWithBody bodyDecoder updateHandler }


delete
  :: forall id
   . (Request.Request id Unit -> Aff HTTPure.Response)
  -> Endpoint id
  -> Endpoint id
delete deleteHandler handlers =
  handlers { delete = instanceHandler deleteHandler }


collectionGET
  :: forall id
   . HTTPure.Path
  -> (Request.Request Unit Unit -> Aff HTTPure.Response)
  -> Endpoint id
  -> Endpoint id
collectionGET prefix getHandler handlers =
  handlers { collectionHandlers =
                Array.snoc handlers.collectionHandlers subPathHandler
           }
  where
    subPathHandler =
      SubPathHandler HTTPure.Get prefix $ collectionHandler getHandler

collectionPOST
  :: forall id requestBody
   . Decode requestBody
  => HTTPure.Path
  -> (Request.Request Unit requestBody -> Aff HTTPure.Response)
  -> Endpoint id
  -> Endpoint id
collectionPOST =
  collectionPOST' (Either.hush <<< Except.runExcept <<< decodeJSON)

collectionPOST'
  :: forall id requestBody
   . (String -> Maybe requestBody)
  -> HTTPure.Path
  -> (Request.Request Unit requestBody -> Aff HTTPure.Response)
  -> Endpoint id
  -> Endpoint id
collectionPOST' bodyDecoder prefix postHandler handlers =
  handlers { collectionHandlers =
                Array.snoc handlers.collectionHandlers subPathHandler
           }
  where
    subPathHandler =
      SubPathHandler HTTPure.Post prefix $
        collectionHandlerWithBody bodyDecoder postHandler


instanceGET
  :: forall id
   . HTTPure.Path
  -> (Request.Request id Unit -> Aff HTTPure.Response)
  -> Endpoint id
  -> Endpoint id
instanceGET prefix getHandler handlers =
  handlers { instanceHandlers =
                Array.snoc handlers.instanceHandlers subPathHandler
           }
  where
    subPathHandler =
      SubPathHandler HTTPure.Get prefix $ instanceHandler getHandler

instancePOST
  :: forall id requestBody
   . Decode requestBody
  => HTTPure.Path
  -> (Request.Request id requestBody -> Aff HTTPure.Response)
  -> Endpoint id
  -> Endpoint id
instancePOST =
  instancePOST' (Either.hush <<< Except.runExcept <<< decodeJSON)

instancePOST'
  :: forall id requestBody
   . (String -> Maybe requestBody)
  -> HTTPure.Path
  -> (Request.Request id requestBody -> Aff HTTPure.Response)
  -> Endpoint id
  -> Endpoint id
instancePOST' bodyDecoder prefix postHandler handlers =
  handlers { instanceHandlers =
                Array.snoc handlers.instanceHandlers subPathHandler
           }
  where
    subPathHandler =
      SubPathHandler HTTPure.Post prefix $
        instanceHandlerWithBody bodyDecoder postHandler


run
  :: forall id
   . Endpoint id
  -> HTTPure.Request
  -> Aff HTTPure.Response
run ep request =
  Maybe.fromMaybe HTTPure.notFound $
    runStandardHandler ep request
      <|> runInstanceHandler ep request
      <|> runCollectionHandler ep request


-- Helpers

collectionHandler
  :: (Request.Request Unit Unit -> Aff HTTPure.Response)
  -> HTTPure.Request
  -> Aff HTTPure.Response
collectionHandler handler httpureRequest =
  case Request.fromRequest (const $ Just unit) unit httpureRequest of
    Nothing -> HTTPure.badRequest ""  -- Shouldn't happen
    Just request -> handler request


collectionHandlerWithBody
  :: forall requestBody
   . (String -> Maybe requestBody)
  -> (Request.Request Unit requestBody -> Aff HTTPure.Response)
  -> HTTPure.Request
  -> Aff HTTPure.Response
collectionHandlerWithBody bodyDecoder handler httpureRequest =
  case Request.fromRequest bodyDecoder unit httpureRequest of
    Nothing -> HTTPure.badRequest "TODO" -- TODO
    Just request -> handler request


instanceHandler
  :: forall id
   . (Request.Request id Unit -> Aff HTTPure.Response)
  -> id
  -> HTTPure.Request
  -> Aff HTTPure.Response
instanceHandler handler id httpureRequest =
  case Request.fromRequest (const $ Just unit) id httpureRequest of
    Nothing -> HTTPure.badRequest "TODO"  -- TODO
    Just request -> handler request


instanceHandlerWithBody
  :: forall id requestBody
   . (String -> Maybe requestBody)
  -> (Request.Request id requestBody -> Aff HTTPure.Response)
  -> id
  -> HTTPure.Request
  -> Aff HTTPure.Response
instanceHandlerWithBody bodyDecoder handler id httpureRequest@{ body } =
  case Request.fromRequest bodyDecoder id httpureRequest of
    Nothing -> HTTPure.badRequest "TODO" -- TODO
    Just request -> handler request


runStandardHandler
  :: forall id
   . Endpoint id
  -> HTTPure.Request
  -> Maybe (Aff HTTPure.Response)
runStandardHandler ep request@{ path, method } =
  handle method path
  where
    handle HTTPure.Get [] = Just $ ep.list request
    handle HTTPure.Post [] = Just $ ep.create request
    handle HTTPure.Get [id] = withId id ep.read
    handle HTTPure.Put [id] = withId id ep.update
    handle HTTPure.Delete [id] = withId id ep.delete
    handle _ _ = Nothing

    withId idString handler =
      handler <$> ep.readId idString <*> Just request


runInstanceHandler
  :: forall id
   . Endpoint id
  -> HTTPure.Request
  -> Maybe (Aff HTTPure.Response)
runInstanceHandler ep request = do
  id <- Array.head request.path >>= ep.readId
  let subPath = Maybe.fromMaybe [] $ Array.tail request.path
  handle request.method subPath id

  where
    handle :: HTTPure.Method -> HTTPure.Path -> id -> Maybe (Aff HTTPure.Response)
    handle HTTPure.Get [] id = Just $ ep.read id request
    handle HTTPure.Put [] id = Just $ ep.update id request
    handle HTTPure.Delete [] id = Just $ ep.delete id request
    handle method subPath id = do
      handler <- findHandler method subPath ep.instanceHandlers
      pure $ handler id request


runCollectionHandler
  :: forall id
   . Endpoint id
  -> HTTPure.Request
  -> Maybe (Aff HTTPure.Response)
runCollectionHandler ep request = do
  handle request.method request.path

  where
    handle HTTPure.Get [] = Just $ ep.list request
    handle HTTPure.Post [] = Just $ ep.create request
    handle method path = do
      handler <- findHandler method request.path ep.collectionHandlers
      pure $ handler request


findHandler
  :: forall handler
   . HTTPure.Method
  -> HTTPure.Path
  -> Array (SubPathHandler handler)
  -> Maybe handler
findHandler method subPath handlers = do
  (SubPathHandler _ _ h) <- Array.find pred handlers
  pure h
  where
    pred (SubPathHandler method' prefix _) =
      method' == method && startsWith prefix

    startsWith prefix =
      Array.length subPath >= Array.length prefix &&
      Array.take (Array.length prefix) subPath == prefix
