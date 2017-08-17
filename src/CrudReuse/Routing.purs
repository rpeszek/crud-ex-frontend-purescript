module CrudReuse.Routing where

import Prelude
import Control.Alt ((<|>))
import Routing.Match (Match)
import Routing.Match.Class (lit, int, str)
import CrudReuse.ReuseApi (class EntityRoute, Proxy(..), baseUri, displayRoute)
import CrudReuse.Model (KeyT(..))

type HashUri = String

data CrudRoute a
    = ListR 
    | ViewR (KeyT a)
    | EditR (KeyT a)
    | CreateR

instance showCrudRoute :: EntityRoute a => Show (CrudRoute a) where
  show ListR = "List " <> displayRoute (Proxy :: Proxy a) 
  show (ViewR (KeyT id)) = "View " <> displayRoute (Proxy :: Proxy a) <> " " <> show id
  show (EditR (KeyT id)) = "Edit " <> displayRoute (Proxy :: Proxy a) <> " " <> show id
  show CreateR = "Create " <> displayRoute (Proxy :: Proxy a)

crudUri :: forall a. EntityRoute a => CrudRoute a -> HashUri
crudUri ListR  = "#/" <> baseUri (Proxy :: Proxy a) <> "/list"
crudUri (ViewR (KeyT id)) = "#/" <> baseUri (Proxy :: Proxy a) <> "/view/" <> show id
crudUri (EditR (KeyT id)) = "#/" <> baseUri (Proxy :: Proxy a) <> "/edit/" <> show id
crudUri CreateR  = "#/" <> baseUri (Proxy :: Proxy a) <> "/create"


crudRoute :: forall a. EntityRoute a => Match (CrudRoute a)
crudRoute = list
        <|> view
        <|> edit
        <|> create
   where
      list = ListR <$  (homeSlash *> lit crudBaseUri *> lit "list")
      view = ViewR <$> (homeSlash *> lit crudBaseUri *> lit "view" *> aKey)
      edit = EditR <$> (homeSlash *> lit crudBaseUri *> lit "edit" *> aKey)
      create = CreateR <$ (homeSlash *> lit crudBaseUri *> lit "create")
      crudBaseUri = baseUri (Proxy :: Proxy a)
      aKey :: Match (KeyT a)
      aKey = KeyT <$> int

msgUri :: String -> HashUri 
msgUri s = "#/msg/" <> s

msgRoute :: Match String 
msgRoute = homeSlash *> lit "msg" *> str

homeSlash :: Match Unit
homeSlash = lit ""
