module CrudReuse.Routing where

import Prelude
import CrudReuse.Common
import Control.Alt ((<|>))
import CrudReuse.Model (KeyT(..))
import Routing.Match (Match)
import Routing.Match.Class (lit, int, str)

data CrudRoute a
    = ListR 
    | ViewR (KeyT a)
    | EditR (KeyT a)

--TODO
instance showCrudRoute :: EntityRoute a => Show (CrudRoute a) where
  show ListR = "List " <> displayRoute (Proxy :: Proxy a) 
  show (ViewR (KeyT id)) = "View " <> displayRoute (Proxy :: Proxy a) <> " " <> show id
  show (EditR (KeyT id)) = "Edit " <> displayRoute (Proxy :: Proxy a) <> " " <> show id

crudUri :: forall a. EntityRoute a => CrudRoute a -> String
crudUri ListR  = "#/" <> baseUri (Proxy :: Proxy a) <> "/list"
crudUri (ViewR (KeyT id)) = "#/" <> baseUri (Proxy :: Proxy a) <> "/view/" <> show id
crudUri (EditR (KeyT id)) = "#/" <> baseUri (Proxy :: Proxy a) <> "/edit/" <> show id


crudRoute :: forall a. EntityRoute a => Match (CrudRoute a)
crudRoute = list
        <|> view
        <|> edit
   where
      list = ListR <$  (homeSlash *> lit crudBaseUri *> lit "list")
      view = ViewR <$> (homeSlash *> lit crudBaseUri *> lit "view" *> aKey)
      edit = EditR <$> (homeSlash *> lit crudBaseUri *> lit "edit" *> aKey)
      crudBaseUri = baseUri (Proxy :: Proxy a)
      aKey :: Match (KeyT a)
      aKey = KeyT <$> int

msgUri :: String -> String 
msgUri s = "#/msg/" <> s

msgRoute :: Match String 
msgRoute = homeSlash *> lit "msg" *> str

homeSlash :: Match Unit
homeSlash = lit ""
