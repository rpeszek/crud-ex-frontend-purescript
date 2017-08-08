module CrudReuse.Model where

import Prelude
--import Control.Alt (alt)
--import Control.Biapplicative (bipure)
--import Control.Monad.Except (throwError)
--import DOM.HTML.HTMLElement (offsetHeight)
import Data.Argonaut (getField, toObject) --Json
--import Data.Argonaut.Core (JObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
--import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
--import Data.Foreign (F)
--import Data.Generic (class Generic, gShow)
import Data.Maybe (Maybe(..))
--import Data.Tuple (Tuple(..))
--import Network.HTTP.Affjax.Response (class Respondable, fromResponse, responseType)
import Prim (Int, String)
  
data KeyT a = KeyT Int
    

instance decodeJsonKeyT :: DecodeJson (KeyT a) where
  decodeJson j = KeyT <$> decodeJson j
instance encodeJsonKeyT :: EncodeJson (KeyT a) where
  encodeJson (KeyT id) = encodeJson id
instance showKeyT :: Show a => Show (KeyT a) where 
  show (KeyT i) =  "KeyT(" <> show i <> ")" 

unKey :: forall a . KeyT a -> Int 
unKey (KeyT i) = i

data Entity a b =
    Entity {
      id :: a
    , entity :: b
    }
toEntity :: forall b. Int -> b -> Entity (KeyT b) b 
toEntity id b = Entity {id: KeyT id, entity: b}

-- https://github.com/purescript-contrib/purescript-argonaut/blob/master/examples/Examples/Data/Argonaut/Record.purs
instance decodeJsonEntity :: DecodeJson b => DecodeJson (Entity (KeyT b) b) where
   decodeJson j = do
      -- json <- (decodeJson j) :: Either String Json
       let jObj = toObject j
       case jObj of 
          Nothing -> 
             Left "Invalid JSON"
          Just obj -> do
             kid <- getField obj "id" :: Either String (KeyT b)
             entity <- getField obj "entity" :: Either String b
             pure $ Entity {id: kid, entity: entity}

instance showEntity :: (Show a, Show b) => Show (Entity a b) where
   show (Entity {id:a, entity:b} ) = "Entity(id=" <> show a <> ",entity=" <> show b <> ")"
