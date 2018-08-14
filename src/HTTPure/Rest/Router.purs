module HTTPure.Rest.Router (route) where

import Prelude

import HTTPure as HTTPure
import HTTPure.Router as Router
import HTTPure.Rest.Endpoint (Endpoint)
import HTTPure.Rest.Endpoint as Endpoint

route :: forall id. HTTPure.Path -> Endpoint id -> Router.Route
route path endpoint = Router.route path $ Endpoint.run endpoint
