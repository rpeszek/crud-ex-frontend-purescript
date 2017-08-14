{-
  Backend counterparts currently do not serve this enitity.
  It is here only to play with polymorphic crud design. 
  For example:  does Slot type used to identify child component need to be type parameterized? 
  Same slot for, say View page will appear once per entity. 
  Coproducts are largely position based so probably the answer is no, but having it type specific
  could be a better idea.
-}
module CrudEx.Model.Other where

import Prelude
import CrudReuse.Server as Serv
import Data.Argonaut.Generic.Aeson as Generic
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import CrudReuse.Model (Entity(..), KeyT(..))
import CrudReuse.ReuseApi (ServerErrM, EditQuery(SetVal), class EntityEditHTML, editView, class EntityBuilder, empty, setFieldValue, class EntityGET, class EntityREST, class EntityReadHTML, class EntityRoute, AppErrM, EntityURI, baseUri, deleteEntity, displayRoute, getEntities, getEntity, listView, postEntity, putEntity, readView)
import CrudReuse.Routing (CrudRoute(..), crudUri)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Array (find)
import Data.Either (Either(..))
import Data.Generic (class Generic, gShow)
import Data.Maybe (Maybe, maybe)
import Network.HTTP.Affjax.Response (ResponseType(..))
import Prim (Int, String)


data Other =
    Other {
      name :: String
    }

derive instance genericOther :: Generic Other
instance decodeJsonOther :: DecodeJson Other where
  decodeJson = Generic.decodeJson
instance encodeJsonOther :: EncodeJson Other where
  encodeJson = Generic.encodeJson
instance showOther :: Show Other where
  show = gShow

instance htmlReadOther :: EntityReadHTML Other where
   readView (Entity obj) =
      let Other obj = obj.entity :: Other
      in HH.div_ [
          HH.div_ [ HH.text "name", HH.text obj.name ] 
      ]
   listView (Entity obj) = 
      let Other o = obj.entity :: Other
      in HH.div_ [ HH.a [ HP.href $ crudUri (ViewR obj.id)] [HH.text (o.name)] ]

instance otherRoute :: EntityRoute Other where
   baseUri _ = fakeURI
   displayRoute _ = "Other"

instance serverGet :: EntityGET e Other where
   getEntities = getListFake
   getEntity = getSingleFake

instance serverREST :: EntityREST e Other where
   postEntity _ = pure $ Left "not implemented"
   putEntity _ _ = pure $ Left "not implemented"
   deleteEntity _ = pure $ Left "not implemented"

instance entityBuilder :: EntityBuilder Other where
  empty = Other {name: ""}
  setFieldValue key value (Other obj) = 
        case key of 
          "name" -> Right $ Other obj{ name = value}
          _ -> Left $ "Invalid key " <> key

instance htmlEdit :: EntityEditHTML Other where
  editView (Other obj) = 
                     HH.div_ $
                      [ HH.label_
                           [ HH.div_ [ HH.text "name:" ]
                          , HH.input
                              [ HP.value obj.name
                                , HE.onValueInput (HE.input $ SetVal "name")
                              ]
                           ]
                      ]


type OtherEntity = Entity (KeyT Other) Other

fakeURI :: EntityURI
fakeURI = "others"

otherStore :: Array OtherEntity
otherStore = [
  Entity {id: (KeyT 0), entity: Other {name: "other 0"}}
  , Entity {id: KeyT 1, entity: Other {name: "other 1"}}
]

getListFake :: forall e. ServerErrM e (Array OtherEntity)
getListFake = pure $ Right otherStore

getSingleFake :: forall e. KeyT Other -> ServerErrM e Other
getSingleFake key = 
    let maybeEnt = find (\(Entity obj) -> obj.id == key) otherStore
        res :: Either String Other
        res = maybe (Left ("item not found " <> show key)) (\(Entity obj) -> Right obj.entity) maybeEnt 
    in pure $ res
