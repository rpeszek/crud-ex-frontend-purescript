module CrudReuse.Common (
  AppErrM
 , AppM
 , ServerErrM
 , ServerM
 , class EntityGET
 , getEntities
 , getEntity
 , class EntityREST
 , putEntity
 , postEntity
 , deleteEntity
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
import CrudReuse.Effect.Navigation (NAVIGATION)
import CrudReuse.Model (Entity, KeyT)
import Data.Either (Either)
import Network.HTTP.Affjax (AJAX)

type AppM eff = (Aff (ajax :: AJAX, appconf:: APPCONFIG, nav:: NAVIGATION | eff))
type AppErrM e a = AppM e (Either String a)

-- | TODO why do I need NAVIGATION here?
type ServerM eff = (Aff (ajax :: AJAX, appconf:: APPCONFIG, nav:: NAVIGATION | eff))
type ServerErrM e a = ServerM e (Either String a)
  
{-
 It would be cleaner if I could define this for arbitrary monad effect:
class EntityGET e a where
  getEntities :: e (Array (Entity(KeyT a) a)) 

 but I do not know how to do lambda level expressions in purescript (functional dependencies?)
 code like 
instance restGet :: EntityGET (AppErrM e) Thing where ...
does not compile
-}
class EntityGET e a where
  getEntities :: ServerErrM e (Array (Entity(KeyT a) a)) 
  getEntity :: KeyT a -> ServerErrM e a 

class (EntityGET e a) <= EntityREST e a where
  postEntity :: a -> ServerErrM e (Entity(KeyT a) a)
  putEntity :: KeyT a -> a -> ServerErrM e a
  deleteEntity :: KeyT a -> ServerErrM e Unit

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
