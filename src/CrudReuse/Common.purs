module CrudReuse.Common (
  AjaxErrM
 , AjaxM
 , class EntityGET
 , getEntities
 , getEntity
 , class EntityReadHTML
 , readView
 , listView
 , Proxy(..)
 , class EntityRoute
 , baseUri
 , displayRoute
 , EntityURI
) where

import Prelude
import Halogen.HTML as HH
import Control.Monad.Aff (Aff)
import CrudReuse.Effect.AppConfig (APPCONFIG)
import CrudReuse.Model (Entity, KeyT)
import Data.Either (Either)
import Network.HTTP.Affjax (AJAX)

type AjaxM eff = (Aff (ajax :: AJAX, appconf:: APPCONFIG | eff))
type AjaxErrM e a = Aff (ajax :: AJAX, appconf:: APPCONFIG | e) (Either String a)
  
{-
 It would be cleaner if I could define this for arbitrary monad effect:
class EntityGET e a where
  getEntities :: e (Array (Entity(KeyT a) a)) 

 but I do not know how to do lambda level expressions in purescript (functional dependencies?)
 code like 
instance restGet :: EntityGET (AjaxErrM e) Thing where ...
does not compile
-}
class EntityGET e a where
  getEntities :: AjaxErrM e (Array (Entity(KeyT a) a)) 
  getEntity :: KeyT a -> AjaxErrM e a 

class (EntityGET e a) <= EntityREST e a where
  postEntity :: a -> AjaxErrM e (Entity(KeyT a) a)
  putEntity :: KeyT a -> a -> AjaxErrM e a
  deleteEntity :: KeyT a -> AjaxErrM e Unit

class EntityRoute a where
   baseUri :: Proxy a -> String
   displayRoute :: Proxy a -> String

class EntityReadHTML a where 
   readView :: forall p i. Entity (KeyT a) a -> HH.HTML p i
   listView :: forall p i. Entity (KeyT a) a -> HH.HTML p i

class EntityReadHTML a <= EntityEditHTML a where 
   editView :: forall p i. Entity (KeyT a) a -> HH.HTML p i

data Proxy a = Proxy

type EntityURI = String
