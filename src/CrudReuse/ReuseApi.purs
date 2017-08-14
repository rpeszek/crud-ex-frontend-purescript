module CrudReuse.ReuseApi (
  AppErrM
 , AppM
 , ServerErrM
 , ServerM
 , class EntityGET, getEntities, getEntity
 , class EntityREST, putEntity, postEntity, deleteEntity, putEntity_
 , class EntityReadHTML, readView, listView
 , class EntityEditHTML, editView
 , Proxy(..)
 , class EntityRoute, baseUri, displayRoute
 , class EntityBuilder, empty, setFieldValue
 , EntityURI
 , EditInput(..)
 , EditQuery(..)
) where

import Prelude
import Halogen.HTML as HH
import Control.Monad.Aff (Aff)
import CrudReuse.Effect.AppConfig (APPCONFIG)
import CrudReuse.Effect.Navigation (NAVIGATION)
import CrudReuse.Model (Entity, KeyT, toEntity_)
import Data.Either (Either)
import Network.HTTP.Affjax (AJAX)

type AppM eff = (Aff (ajax :: AJAX, appconf:: APPCONFIG, nav:: NAVIGATION | eff))
type AppErrM e a = AppM e (Either String a)

{-
  Ideally ServerM should not list NAVIGATION effect but this breaks compilation. 
  Looks like compiler bug ??
  it has problem coercing Aff(eff) to Aff(nav :: NAVIGATION| eff)
  in the presence of type classes like EntityREST
  liftAff works in some places and not in others
-}
type ServerM eff = AppM eff -- (Aff (ajax :: AJAX, appconf:: APPCONFIG | eff))
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
class EntityGET e model where
  getEntities :: ServerErrM e (Array (Entity(KeyT model) model)) 
  getEntity :: KeyT model -> ServerErrM e model 

class (EntityGET e model) <= EntityREST e model where
  postEntity :: model -> ServerErrM e (Entity(KeyT model) model)
  putEntity :: KeyT model -> model -> ServerErrM e model
  deleteEntity :: KeyT model -> ServerErrM e Unit

putEntity_ :: forall e model. EntityREST e model => KeyT model -> model -> ServerErrM e (Entity(KeyT model) model)
putEntity_ key model = do
     errOrModel <- putEntity key model
     pure $ (toEntity_ key) <$> errOrModel

class EntityRoute model where
   baseUri :: Proxy model -> String
   displayRoute :: Proxy model -> String

class EntityReadHTML model where 
   readView :: forall p i. Entity (KeyT model) model -> HH.HTML p i
   listView :: forall p i. Entity (KeyT model) model -> HH.HTML p i

class EntityEditHTML model where 
   editView :: forall p. model -> HH.HTML p (EditQuery model Unit)

-- | TODO would be cool to derive instance of this for Generic
class EntityBuilder model where
   empty :: model
   -- | key -> value -> mod model fn
   setFieldValue :: String -> String -> model -> Either String model

data EditInput model = Retrieve (KeyT model) | Empty

data EditQuery model a
  = Set (EditInput model) a
   | SetVal String String a
   | Save a


data Proxy a = Proxy

type EntityURI = String
