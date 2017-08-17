module CrudEx.Routing where
  
import Prelude
import Control.Alt ((<|>))
import CrudEx.Model.Other (Other)
import CrudEx.Model.Thing (Thing)
import CrudReuse.Routing (HashUri, CrudRoute, crudUri, crudRoute, msgRoute, msgUri)
import Routing.Match (Match)

data AppRoute   
       = ThingR (CrudRoute Thing)
       | OtherR (CrudRoute Other)
       | MsgR String

instance showAppRoute ::  Show (AppRoute) where
  show (ThingR r) = "ThingR " <> show r 
  show (OtherR r) = "OtherR " <> show r 
  show (MsgR str) = "MsgR " <> str

appUri :: AppRoute -> HashUri
appUri (ThingR r) = crudUri r 
appUri (OtherR r) = crudUri r
appUri (MsgR r)   = msgUri r

appRoute :: Match AppRoute
appRoute = ThingR <$> crudRoute
       <|> OtherR <$> crudRoute
       <|> MsgR   <$> msgRoute
