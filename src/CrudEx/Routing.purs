module CrudEx.Routing where
  
import Prelude
import Control.Alt ((<|>))
import CrudEx.Model.Other (Other)
import CrudEx.Model.Thing (Thing)
--import CrudReuse.Common (class EntityRoute, baseUri, displayRoute)
import CrudReuse.Routing (CrudRoute, crudUri, crudRoute, msgRoute, msgUri)
--import Network.HTTP.Affjax.Response (ResponseType(..))
import Routing.Match (Match)
--import Routing.Match.Class (lit, str)

data AppRoute   
       = ThingR (CrudRoute Thing)
       | OtherR (CrudRoute Other)
       | MsgR String

instance showAppRoute ::  Show (AppRoute) where
  show (ThingR r) = "ThingR " <> show r 
  show (OtherR r) = "OtherR " <> show r 
  show (MsgR str) = "MsgR " <> str

appRoute :: Match AppRoute
appRoute = ThingR <$> crudRoute
       <|> OtherR <$> crudRoute
       <|> MsgR   <$> msgRoute


appUri :: AppRoute -> String
appUri (ThingR r) = crudUri r 
appUri (OtherR r) = crudUri r
appUri (MsgR r)   = msgUri r
