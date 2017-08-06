module CrudEx.Model where

import Prelude
import CrudReuse.Server as Serv
import Data.Argonaut.Generic.Aeson as Generic
import Halogen.HTML as HH
import CrudReuse.Common (class EntityReadHTML, detailedView, listView, class EntityGET, getEntities, getEntity)
import CrudReuse.Model (Entity(..), KeyT)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Generic (class Generic, gShow)
import Data.Maybe (Maybe)
import Network.HTTP.Affjax.Response (ResponseType(..))
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
   detailedView thingEntity = HH.div_ []
   listView (Entity obj) = 
      let Thing thing = obj.entity :: Thing
      in HH.div_ [ HH.text (thing.name) ]
instance serverGet :: EntityGET e Thing where
   getEntities = Serv.getList thingsURI
   getEntity = Serv.getSingle thingsURI

type ThingEntity = Entity (KeyT Thing) Thing

thingsURI :: Serv.EntityURI
thingsURI = "things"

{-
test :: String
test = "[{\"id\":0,\"entity\":{\"userId\":null,\"name\":\"testName1\",\"description\":\"testDesc1\"}},{\"id\":1,\"entity\":{\"userId\":null,\"name\":\"testName2\",\"description\":\"testDesc2\"}}]"

testP :: Either String (List ThingEntity)
testP =  do 
            json <- jsonParser test
            decodeJson json

test2 :: String
test2 = "{\"id\":0,\"entity\":{\"userId\":null,\"name\":\"testName1\",\"description\":\"testDesc1\"}}"

test2P :: Either String (ThingEntity)
test2P =  do 
            json <- jsonParser test2
            decodeJson json
-}
