module HTTPure.Contrib.Rest.Endpoint
  ( Endpoint
  , EndpointOptions
  , BodyErrors
  , endpoint
  , list
  , create
  , create'
  , read
  , update
  , update'
  , delete
  , collectionRoute
  , collectionRouteWithBody, collectionRouteWithBody'
  , instanceRoute
  , instanceRouteWithBody, instanceRouteWithBody'
  , run
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except as Except
import Data.Array as Array
import Data.Either (Either(..))
import Data.Either as Either
import Data.Foldable as Foldable
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Effect.Aff (Aff)
import Foreign as Foreign
import Foreign.Class (class Decode)
import Foreign.Generic (decodeJSON)
import HTTPure as HTTPure
import HTTPure.Contrib.Rest.Request as Request
import Partial.Unsafe (unsafePartial)

type BodyErrors = Foreign.MultipleErrors

type CollectionHandler = HTTPure.Request -> Aff HTTPure.Response
type InstanceHandler id = id -> HTTPure.Request -> Aff HTTPure.Response
data SubPathHandler a = SubPathHandler HTTPure.Method HTTPure.Path a


type EndpointOptions id =
  { readId :: String -> Maybe id
  , bodyError :: BodyErrors -> Aff HTTPure.Response
  }


defaultBodyError :: BodyErrors -> Aff HTTPure.Response
defaultBodyError errors =
  HTTPure.badRequest'
    (HTTPure.header "Content-Type" "text/plain")
    (Foldable.intercalate "\n" $ Foreign.renderForeignError <$> errors)


data Endpoint id = Endpoint
  { list :: CollectionHandler
  , create :: CollectionHandler
  , read :: InstanceHandler id
  , update :: InstanceHandler id
  , delete :: InstanceHandler id
  , collectionHandlers :: Array (SubPathHandler CollectionHandler)
  , instanceHandlers :: Array (SubPathHandler (InstanceHandler id))
  , readId :: String -> Maybe id
  , bodyError :: BodyErrors -> Aff HTTPure.Response
  }


endpoint :: Endpoint Int
endpoint =
  endpoint'
    { readId: Int.fromString
    , bodyError: defaultBodyError
    }


endpoint' :: forall id. EndpointOptions id -> Endpoint id
endpoint' { readId, bodyError } =
  Endpoint
    { list: collectionNotFound
    , create: collectionNotFound
    , read: instanceNotFound
    , update: instanceNotFound
    , delete: instanceNotFound
    , collectionHandlers: []
    , instanceHandlers: []
    , readId
    , bodyError
    }
  where
    collectionNotFound _ = HTTPure.notFound
    instanceNotFound _ _ = HTTPure.notFound


list
  :: forall id
   . (Request.Request Unit Unit -> Aff HTTPure.Response)
  -> Endpoint id
  -> Endpoint id
list listHandler (Endpoint ep) =
  Endpoint $ ep { list = collectionHandler listHandler }


create
  :: forall id requestBody
   . Decode requestBody
  => (Request.Request Unit requestBody -> Aff HTTPure.Response)
  -> Endpoint id
  -> Endpoint id
create =
  create' (Except.runExcept <<< decodeJSON)


create'
  :: forall id requestBody
   . (String -> Either BodyErrors requestBody)
  -> (Request.Request Unit requestBody -> Aff HTTPure.Response)
  -> Endpoint id
  -> Endpoint id
create' bodyDecoder createHandler (Endpoint ep) =
  Endpoint $ ep { create = collectionHandlerWithBody bodyDecoder ep.bodyError createHandler }


read
  :: forall id
   . (Request.Request id Unit -> Aff HTTPure.Response)
  -> Endpoint id
  -> Endpoint id
read readHandler (Endpoint ep) =
  Endpoint $ ep { read = instanceHandler readHandler }


update
  :: forall id requestBody
   . Decode requestBody
  => (Request.Request id requestBody -> Aff HTTPure.Response)
  -> Endpoint id
  -> Endpoint id
update =
  update' (Except.runExcept <<< decodeJSON)


update'
  :: forall id requestBody
   . (String -> Either BodyErrors requestBody)
  -> (Request.Request id requestBody -> Aff HTTPure.Response)
  -> Endpoint id
  -> Endpoint id
update' bodyDecoder updateHandler (Endpoint ep) =
  Endpoint $ ep { update = instanceHandlerWithBody bodyDecoder ep.bodyError updateHandler }


delete
  :: forall id
   . (Request.Request id Unit -> Aff HTTPure.Response)
  -> Endpoint id
  -> Endpoint id
delete deleteHandler (Endpoint ep) =
  Endpoint $ ep { delete = instanceHandler deleteHandler }


collectionRoute
  :: forall id
   . HTTPure.Path
  -> HTTPure.Method
  -> (Request.Request Unit Unit -> Aff HTTPure.Response)
  -> Endpoint id
  -> Endpoint id
collectionRoute prefix method handler (Endpoint ep) =
  Endpoint $
    ep { collectionHandlers =
            Array.snoc ep.collectionHandlers subPathHandler
       }
  where
    subPathHandler =
      SubPathHandler method prefix $ collectionHandler handler

collectionRouteWithBody
  :: forall id requestBody
   . Decode requestBody
  => HTTPure.Path
  -> HTTPure.Method
  -> (Request.Request Unit requestBody -> Aff HTTPure.Response)
  -> Endpoint id
  -> Endpoint id
collectionRouteWithBody =
  collectionRouteWithBody' (Except.runExcept <<< decodeJSON)


collectionRouteWithBody'
  :: forall id requestBody
   . (String -> Either BodyErrors requestBody)
  -> HTTPure.Path
  -> HTTPure.Method
  -> (Request.Request Unit requestBody -> Aff HTTPure.Response)
  -> Endpoint id
  -> Endpoint id
collectionRouteWithBody' bodyDecoder prefix method handler (Endpoint ep) =
  Endpoint $
    ep { collectionHandlers =
            Array.snoc ep.collectionHandlers subPathHandler
       }
  where
    subPathHandler =
      SubPathHandler method prefix $
        collectionHandlerWithBody bodyDecoder ep.bodyError handler


instanceRoute
  :: forall id
   . HTTPure.Path
  -> HTTPure.Method
  -> (Request.Request id Unit -> Aff HTTPure.Response)
  -> Endpoint id
  -> Endpoint id
instanceRoute prefix method handler (Endpoint ep) =
  Endpoint $
    ep { instanceHandlers =
            Array.snoc ep.instanceHandlers subPathHandler
       }
  where
    subPathHandler =
      SubPathHandler method prefix $ instanceHandler handler


instanceRouteWithBody
  :: forall id requestBody
   . Decode requestBody
  => HTTPure.Path
  -> HTTPure.Method
  -> (Request.Request id requestBody -> Aff HTTPure.Response)
  -> Endpoint id
  -> Endpoint id
instanceRouteWithBody =
  instanceRouteWithBody' (Except.runExcept <<< decodeJSON)


instanceRouteWithBody'
  :: forall id requestBody
   . (String -> Either BodyErrors requestBody)
  -> HTTPure.Path
  -> HTTPure.Method
  -> (Request.Request id requestBody -> Aff HTTPure.Response)
  -> Endpoint id
  -> Endpoint id
instanceRouteWithBody' bodyDecoder prefix method postHandler (Endpoint ep) =
  Endpoint $
    ep { instanceHandlers =
            Array.snoc ep.instanceHandlers subPathHandler
       }
  where
    subPathHandler =
      SubPathHandler method prefix $
        instanceHandlerWithBody bodyDecoder ep.bodyError postHandler


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
  handler $
    unsafePartial Either.fromRight $
      Request.fromRequest (const $ Right unit) unit httpureRequest


collectionHandlerWithBody
  :: forall requestBody
   . (String -> Either BodyErrors requestBody)
  -> (BodyErrors -> Aff HTTPure.Response)
  -> (Request.Request Unit requestBody -> Aff HTTPure.Response)
  -> HTTPure.Request
  -> Aff HTTPure.Response
collectionHandlerWithBody bodyDecoder bodyError handler httpureRequest =
  case Request.fromRequest bodyDecoder unit httpureRequest of
    Left errors -> bodyError errors
    Right request -> handler request


instanceHandler
  :: forall id
   . (Request.Request id Unit -> Aff HTTPure.Response)
  -> id
  -> HTTPure.Request
  -> Aff HTTPure.Response
instanceHandler handler id httpureRequest =
  handler $
    unsafePartial Either.fromRight $
      Request.fromRequest (const $ Right unit) id httpureRequest


instanceHandlerWithBody
  :: forall id requestBody
   . (String -> Either BodyErrors requestBody)
  -> (BodyErrors -> Aff HTTPure.Response)
  -> (Request.Request id requestBody -> Aff HTTPure.Response)
  -> id
  -> HTTPure.Request
  -> Aff HTTPure.Response
instanceHandlerWithBody bodyDecoder bodyError handler id httpureRequest@{ body } =
  case Request.fromRequest bodyDecoder id httpureRequest of
    Left err -> bodyError err
    Right request -> handler request


runStandardHandler
  :: forall id
   . Endpoint id
  -> HTTPure.Request
  -> Maybe (Aff HTTPure.Response)
runStandardHandler (Endpoint ep) request@{ path, method } =
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
runInstanceHandler (Endpoint ep) request = do
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
runCollectionHandler (Endpoint ep) request = do
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
