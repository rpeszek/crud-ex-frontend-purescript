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
import Halogen.HTML.Properties as HP
import CrudReuse.Common (AjaxErrM, class EntityGET, class EntityReadHTML, class EntityRoute, baseUri, displayRoute, getEntities, getEntity, listView, readView, EntityURI)
import CrudReuse.Model (Entity(..), KeyT(..))
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
      let Other thing = obj.entity :: Other
      in HH.div_ [
          HH.div_ [ HH.text "name", HH.text thing.name ] 
      ]
   listView (Entity obj) = 
      let Other thing = obj.entity :: Other
      in HH.div_ [ HH.a [ HP.href $ crudUri (ViewR obj.id)] [HH.text (thing.name)] ]

instance thingRoute :: EntityRoute Other where
   baseUri _ = fakeURI
   displayRoute _ = "Other"

instance serverGet :: EntityGET e Other where
   getEntities = getListFake
   getEntity = getSingleFake

type OtherEntity = Entity (KeyT Other) Other

fakeURI :: EntityURI
fakeURI = "others"

otherStore :: Array OtherEntity
otherStore = [
  Entity {id: (KeyT 0), entity: Other {name: "other 0"}}
  , Entity {id: KeyT 1, entity: Other {name: "other 1"}}
]

getListFake :: forall e. AjaxErrM e (Array OtherEntity)
getListFake = pure $ Right otherStore

getSingleFake :: forall e. KeyT Other -> AjaxErrM e Other
getSingleFake key = 
    let maybeEnt = find (\(Entity obj) -> obj.id == key) otherStore
        res :: Either String Other
        res = maybe (Left ("item not found " <> show key)) (\(Entity obj) -> Right obj.entity) maybeEnt 
    in pure $ res
