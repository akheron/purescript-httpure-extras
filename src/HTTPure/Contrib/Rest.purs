module HTTPure.Contrib.Rest
  ( module HTTPure.Contrib.Rest.Endpoint
  , module HTTPure.Contrib.Rest.Request
  , module HTTPure.Contrib.Rest.Response
  , module HTTPure.Contrib.Rest.Router
  ) where

import HTTPure.Contrib.Rest.Endpoint
  ( Endpoint
  , endpoint
  , list
  , create, create'
  , read
  , update, update'
  , delete
  )
import HTTPure.Contrib.Rest.Request (Request)
import HTTPure.Contrib.Rest.Response
  ( okJSON
  , badRequestJSON
  )
import HTTPure.Contrib.Rest.Router (route)
