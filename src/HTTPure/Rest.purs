module HTTPure.Rest
  ( module HTTPure.Rest.Endpoint
  , module HTTPure.Rest.Request
  , module HTTPure.Rest.Response
  , module HTTPure.Rest.Router
  ) where

import HTTPure.Rest.Endpoint
  ( Endpoint
  , endpoint
  , list
  , create, create'
  , read
  , update, update'
  , delete
  )
import HTTPure.Rest.Request (Request)
import HTTPure.Rest.Response
  ( okJSON
  , badRequestJSON
  )
import HTTPure.Rest.Router (route)
