module CrudEx.Model.Thing where

import Prelude
import CrudReuse.Server as Serv
import Data.Argonaut.Generic.Aeson as Generic
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import CrudReuse.ReuseApi (EditQuery(SetVal), class EntityEditHTML, editView, class EntityBuilder, empty, setFieldValue, class EntityGET, class EntityREST, class EntityReadHTML, class EntityRoute, baseUri, displayRoute, getEntities, getEntity, postEntity, putEntity, deleteEntity, listView, readView, EntityURI)
import CrudReuse.Model (Entity(..), KeyT(..))
import CrudReuse.Routing (CrudRoute(..), crudUri)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Generic (class Generic, gShow)
import Data.Maybe (Maybe(..))
import Prim (Int, String)


data Thing =
    Thing {
      name :: String
    , description :: String
    , userId :: Maybe Int
    }

derive instance genericThing :: Generic Thing
instance decodeJsonThing :: DecodeJson Thing where
  decodeJson = Generic.decodeJson
instance encodeJsonThing :: EncodeJson Thing where
  encodeJson = Generic.encodeJson
instance showThing :: Show Thing where
  show = gShow

instance htmlReadThing :: EntityReadHTML Thing where
   readView (Entity obj) =
      let Thing thing = obj.entity :: Thing
      in HH.div_ [
          HH.div_ [ HH.text "name", HH.text thing.name ] 
        , HH.div_ [ HH.text "description", HH.text thing.description ]
      ]
   listView (Entity obj) = 
      let Thing thing = obj.entity :: Thing
      in HH.div_ [ HH.a [ HP.href $ crudUri (ViewR obj.id)] [HH.text (thing.name)] ]

instance thingRoute :: EntityRoute Thing where
   baseUri _ = thingsURI
   displayRoute _ = "Thing"

instance serverGet :: EntityGET e Thing where
   getEntities = Serv.getList thingsURI
   getEntity = Serv.getSingle thingsURI

instance serverREST :: EntityREST e Thing where
   postEntity = Serv.postSingle thingsURI
   putEntity = Serv.putSingle thingsURI
   deleteEntity = Serv.deleteSingle thingsURI

instance entityBuilder :: EntityBuilder Thing where
  empty = Thing {name: "", description: "", userId: Nothing}
  setFieldValue key value (Thing obj) = 
        case key of 
          "name" -> Right $ Thing obj{ name = value}
          "description" -> Right $ Thing obj{description = value}
          _ -> Left $ "Invalid key " <> key

instance htmlEdit :: EntityEditHTML Thing where
  editView (Thing obj) = 
                     HH.div_ $
                      [ HH.label_
                           [ HH.div_ [ HH.text "name:" ]
                          , HH.input
                              [ HP.value obj.name
                                , HE.onValueInput (HE.input $ SetVal "name")
                              ]
                           ]
                      , HH.label_
                           [ HH.div_ [ HH.text "description:" ]
                          , HH.input
                              [ HP.value obj.description
                                , HE.onValueInput (HE.input $ SetVal "description")
                              ]
                           ]
                      ]

type ThingEntity = Entity (KeyT Thing) Thing

thingsURI :: EntityURI
thingsURI = "things"
