module CrudReuse.Routing where

import Prelude
import CrudReuse.Common
import Control.Alt ((<|>))
import CrudReuse.Model (KeyT(..))
import Network.HTTP.Affjax.Response (ResponseType(..))
import Routing.Match (Match)
import Routing.Match.Class (lit, int, str)

data CrudRoutes a
    = List 
    | View (KeyT a)
    | Edit (KeyT a)
    | NotDone String -- just to simplify development

--TODO
instance showCrudRoutes :: EntityRoute a => Show (CrudRoutes a) where
  show List = "List " <> displayRoute (Proxy :: Proxy a) 
  show (View (KeyT id)) = "View " <> displayRoute (Proxy :: Proxy a) <> " " <> show id
  show (Edit (KeyT id)) = "Edit " <> displayRoute (Proxy :: Proxy a) <> " " <> show id
  show (NotDone str) = "NotDone " <> displayRoute (Proxy :: Proxy a) <> " " <> str

crudUri :: forall a. EntityRoute a => CrudRoutes a -> String
crudUri List  = "#/" <> baseUri (Proxy :: Proxy a) <> "/list"
crudUri (View (KeyT id)) = "#/" <> baseUri (Proxy :: Proxy a) <> "/view/" <> show id
crudUri (Edit (KeyT id)) = "#/" <> baseUri (Proxy :: Proxy a) <> "/edit/" <> show id
crudUri (NotDone str) = "#/" <> baseUri (Proxy :: Proxy a) <> "/notDone/"  <> str


crudRoute :: forall a. EntityRoute a => Match (CrudRoutes a)
crudRoute = list
      <|> view
      <|> edit
      <|> notDone
  where
    list = List <$ (homeSlash *> lit bcrudUri *> lit "list")
    view = View <$> (homeSlash *> lit bcrudUri *> lit "view" *> aKey)
    edit = Edit <$> (homeSlash *> lit bcrudUri *> lit "edit" *> aKey)
    notDone = NotDone <$> (homeSlash *> lit bcrudUri *> lit "notDone" *> str)
    bcrudUri = baseUri (Proxy :: Proxy a)
    --int :: Match Int
    --int = floor <$> num
    aKey :: Match (KeyT a)
    aKey = KeyT <$> int

msgUri :: String -> String 
msgUri s = "#/msg/" <> s

msgRoute :: Match String 
msgRoute = homeSlash *> lit "msg" *> str

homeSlash :: Match Unit
homeSlash = lit ""
