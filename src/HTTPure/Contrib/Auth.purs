module HTTPure.Contrib.Auth (basicAuth) where

import Prelude

import Data.ArrayBuffer.ArrayBuffer as ArrayBuffer
import Data.Base64 (Base64(..), decodeBase64)
import Data.Either as Either
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import HTTPure ((!!))
import HTTPure as HTTPure

basicAuth
  :: (String -> String -> Boolean)
  -> String
  -> (HTTPure.Request -> Aff HTTPure.Response)
  -> HTTPure.Request
  -> Aff HTTPure.Response
basicAuth verifyUser realm handler request =
  case getAuth request of
    Nothing ->
      unauthorized

    Just (Tuple username password) ->
      if verifyUser username password
      then handler request
      else unauthorized

  where
    unauthorized = HTTPure.unauthorized' $
      HTTPure.header "WWW-Authenticate" ("Basic realm=\"" <> realm <> "\"")


getAuth :: HTTPure.Request -> Maybe (Tuple String String)
getAuth { headers } = do
  authorization <- headers !! "Authorization"
  base64 <- String.stripPrefix (Pattern "Basic ") authorization
  value <- decodeBase64 (Base64 base64) >>= (ArrayBuffer.decodeToString >>> Either.hush)
  semicolon <- String.indexOf (Pattern ":") value
  let { before, after } = String.splitAt semicolon value
  pure $ Tuple before (String.drop 1 after)
