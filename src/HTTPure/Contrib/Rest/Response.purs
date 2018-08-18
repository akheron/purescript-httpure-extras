module HTTPure.Contrib.Rest.Response
  ( okJSON
  , badRequestJSON
  ) where

import Prelude

import Effect.Aff (Aff)
import Foreign.Class (class Encode)
import Foreign.Generic (encodeJSON)

import HTTPure as HTTPure

contentTypeJSON :: HTTPure.Headers
contentTypeJSON =
  HTTPure.header "Content-Type" "application/json"

-- | 200 with JSON body
okJSON
  :: forall body
   . Encode body
  => body
  -> Aff HTTPure.Response
okJSON body =
  HTTPure.ok' contentTypeJSON $ encodeJSON body

-- | 400 with JSON body
badRequestJSON
  :: forall body
   . Encode body
  => body
  -> Aff HTTPure.Response
badRequestJSON body =
  HTTPure.badRequest' contentTypeJSON $ encodeJSON body
