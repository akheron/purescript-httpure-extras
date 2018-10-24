module HTTPure.Contrib.Router (Route, router, route) where

import Prelude

import Data.Array as Array
import Data.Foldable as Foldable
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import HTTPure as HTTPure

data Route = Route HTTPure.Path (HTTPure.Request -> Aff HTTPure.Response)

-- | Construct a `Route` from a prefix and a request handler function.
-- |
-- | The request handler is called with a modified `Request`, with
-- | `prefix` removed from `path`.
route :: HTTPure.Path -> (HTTPure.Request -> Aff HTTPure.Response) -> Route
route = Route

-- | `router` takes an array of routes and returns a function that can
-- | be used as the request handler with `HTTPure.serve`.
-- |
-- | Matches are searched in the order which they are in the array.
-- |
-- | ``` purescript
-- | routes :: Array Route
-- | routes =
-- |   [ route ["baz", "quux"] bazQuuxHandler  -- /baz/quux
-- |   , route ["foo"]         fooHandler      -- /foo
-- |   , route []              indexHandler     -- /
-- |   ]
-- |
-- | main :: Effect Unit
-- | main = do
-- |   serve 8080 (router routes) (pure unit)
router
  :: Array Route
  -> HTTPure.Request
  -> Aff HTTPure.Response
router routes request@{ path } =
  case Foldable.find (\(Route prefix _) -> startsWith prefix) routes of
    Just (Route prefix handler) ->
      handler $ request { path = subpath prefix }
    Nothing ->
      HTTPure.notFound

  where
    startsWith prefix =
      Array.length path >= Array.length prefix &&
      Array.take (Array.length prefix) path == prefix

    subpath prefix =
      Array.drop (Array.length prefix) path
