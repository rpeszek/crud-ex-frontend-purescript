module CrudReuse.Routing where

import Prelude
import Control.Alt ((<|>))
import CrudReuse.Model (KeyT(..))
import Network.HTTP.Affjax.Response (ResponseType(..))
import Routing.Match (Match)
import Routing.Match.Class (lit, int, str)
import CrudReuse.Common

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

uri :: forall a. EntityRoute a => CrudRoutes a -> String
uri List  = "#/" <> baseUri (Proxy :: Proxy a) <> "/list"
uri (View (KeyT id)) = "#/" <> baseUri (Proxy :: Proxy a) <> "/view/" <> show id
uri (Edit (KeyT id)) = "#/" <> baseUri (Proxy :: Proxy a) <> "/edit/" <> show id
uri (NotDone str) = "#/" <> baseUri (Proxy :: Proxy a) <> "/notDone/"  <> str


routing :: forall a. EntityRoute a => Match (CrudRoutes a)
routing = list
      <|> view
      <|> edit
      <|> notDone
  where
    list = List <$ (homeSlash *> lit buri *> lit "list")
    view = View <$> (homeSlash *> lit buri *> lit "view" *> aKey)
    edit = Edit <$> (homeSlash *> lit buri *> lit "edit" *> aKey)
    notDone = NotDone <$> (homeSlash *> lit buri *> lit "notDone" *> str)
    buri = baseUri (Proxy :: Proxy a)
    homeSlash :: Match Unit
    homeSlash = lit ""
    --int :: Match Int
    --int = floor <$> num
    aKey :: Match (KeyT a)
    aKey = KeyT <$> int
