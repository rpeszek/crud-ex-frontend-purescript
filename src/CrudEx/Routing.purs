module CrudEx.Routing where
  
import Prelude
import Control.Alt ((<|>))
import CrudEx.Model.Other (Other)
import CrudEx.Model.Thing (Thing)
--import CrudReuse.Common (class EntityRoute, baseUri, displayRoute)
import CrudReuse.Routing (CrudRoutes, crudUri, crudRoute, msgRoute, msgUri)
--import Network.HTTP.Affjax.Response (ResponseType(..))
import Routing.Match (Match)
--import Routing.Match.Class (lit, str)

data AppRoute = ThingR (CrudRoutes Thing)
              | OtherR (CrudRoutes Other)
              | MsgR String

instance showAppRoute ::  Show (AppRoute) where
  show (ThingR r) = "ThingR " <> show r 
  show (OtherR r) = "OtherR " <> show r 
  show (MsgR str) = "MsgR " <> str

appRoute :: Match AppRoute
appRoute = thing
      <|> other
      <|> msg
  where
    thing = ThingR <$> crudRoute
    other = OtherR <$> crudRoute
    msg = MsgR <$> msgRoute

uri :: AppRoute -> String
uri (ThingR r) = crudUri r 
uri (OtherR r) = crudUri r
uri (MsgR r)   = msgUri r
