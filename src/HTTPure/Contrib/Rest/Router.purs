module HTTPure.Contrib.Rest.Router (route) where

import Prelude

import HTTPure as HTTPure
import HTTPure.Contrib.Router as Router
import HTTPure.Contrib.Rest.Endpoint (Endpoint)
import HTTPure.Contrib.Rest.Endpoint as Endpoint

route :: forall id. HTTPure.Path -> Endpoint id -> Router.Route
route path endpoint = Router.route path $ Endpoint.run endpoint
