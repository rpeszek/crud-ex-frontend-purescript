module CrudEx.Routing where

import Prelude
import Control.Alt ((<|>))
import CrudEx.Model (Thing)
import CrudReuse.Model (KeyT(..))
import Network.HTTP.Affjax.Response (ResponseType(..))
import Routing.Match (Match)
import Routing.Match.Class (lit, int, str)
  
data Routes
  = ThingList
   | ThingView (KeyT Thing)
   | ThingEdit (KeyT Thing)
   | NotDone String

instance showRoutes :: Show Routes where
  show ThingList = "List Things"
  show (ThingView (KeyT id)) = "View Thing" <> show id
  show (ThingEdit (KeyT id)) = "Edit Thing" <> show id
  show (NotDone _) = "Message"

uri :: Routes -> String
uri ThingList = "#/things/list"
uri (ThingView (KeyT id)) = "#/things/view/" <> show id
uri (ThingEdit (KeyT id)) = "#/things/edit/" <> show id
uri (NotDone msg) = "#/notDone/" <> msg


routing :: Match Routes
routing = thingList
      <|> thingView
      <|> thingEdit
      <|> notDone
  where
    thingList = ThingList <$ (homeSlash *> lit "things" *> lit "list")
    thingView = ThingView <$> (homeSlash *> lit "things" *> lit "view" *> thingKey)
    thingEdit = ThingEdit <$> (homeSlash *> lit "things" *> lit "edit" *> thingKey)
    notDone = NotDone <$> (homeSlash *> lit "notDone" *> str)
    oneSlash :: Match Unit
    oneSlash = lit "/"
    homeSlash :: Match Unit
    homeSlash = lit ""
    --int :: Match Int
    --int = floor <$> num
    thingKey :: Match (KeyT Thing)
    thingKey = KeyT <$> int
