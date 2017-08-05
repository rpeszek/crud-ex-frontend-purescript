module CrudEx.Model where

import Prelude
import CrudReuse.Model (Entity(..), KeyT)
import CrudReuse.Server as Serv
import Data.Argonaut.Generic.Aeson as GAeson
import Halogen.HTML as HH
--import CrudReuse.Common (AjaxM)
import CrudReuse.Common (class ViewableEntity, detailedView, listView, class RestfullyGet, getEntities)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Generic (class Generic, gShow)
import Data.Maybe (Maybe)
import Prim (Int, String)


data Thing =
    Thing {
      name :: String
    , description :: String
    , userId :: Maybe Int
    }

derive instance genericThing :: Generic Thing
instance decodeJsonThing :: DecodeJson Thing where
  decodeJson = GAeson.decodeJson
instance encodeJsonThing :: EncodeJson Thing where
  encodeJson = GAeson.encodeJson
instance showThing :: Show Thing where
  show = gShow
instance htmlThing :: ViewableEntity Thing where
   detailedView thingEntity = HH.div_ []
   listView (Entity obj) = 
      let Thing thing = obj.entity :: Thing
      in HH.div_ [ HH.text (thing.name) ]
instance restGet :: RestfullyGet e Thing where
   getEntities = Serv.getElements "things"
     

type ThingEntity = Entity (KeyT Thing) Thing

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
