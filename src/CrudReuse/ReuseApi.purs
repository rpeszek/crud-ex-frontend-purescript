{-|
Module      : ReuseApi
Description : Contains type classes and common type definitions used to achieve 
              CRUD polymorphism.

Design considerations:
A much better design would be:

class Monad m <= EntityGetM m model where
  getEntities :: m Array (Entity(KeyT model) model)) 
  getEntity :: KeyT model -> m model 

class (EntityGetM m model) <= EntityRestM m model where
  postEntity :: model -> m Entity(KeyT model) model)
  putEntity :: KeyT model -> model -> m model
  deleteEntity :: KeyT model -> m Unit

However this is currently problematic if I want (and I do) to use effect rows:
https://github.com/purescript/purescript/issues/1510

This will not compile ("Type class instance head is invalid due to use of <row> type"):
instance serverGet ::  EntityGetM (Aff (ajax :: AJAX, appconf:: APPCONFIG | eff)) Thing where ...

Supposedly, something like this may be possible in the future (purescript enhancing its functional dependencies):

instance serverGet :: (eff ~ (ajax :: AJAX, appconf:: APPCONFIG)) => EntityGetM eff Thing where ...

There is a possible workaround using newtypes:

newtype ServerM eff a = ServerM (Aff (ajax :: AJAX, appconf:: APPCONFIG | eff) a)
instance serverGet :: EntityGetM (ServerM eff) Thing where ...
instance affServer :: ??? => MonadAff e (ServerM e) where ...
??? - is some type class contraint representing effects

but this approach seems like swimming upstream. 
Current approach hardcodes Aff.
-}

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

type ServerM eff = (Aff (ajax :: AJAX, appconf:: APPCONFIG | eff))
type ServerErrM e a = ServerM e (Either String a)
  

-- | Server intraction 
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

-- | Hash routing
class EntityRoute model where
   baseUri :: Proxy model -> String
   displayRoute :: Proxy model -> String

-- | HTML rendering
class EntityReadHTML model where 
   readView :: forall p i. Entity (KeyT model) model -> HH.HTML p i
   listView :: forall p i. Entity (KeyT model) model -> HH.HTML p i

class EntityEditHTML model where 
   editView :: forall p. model -> HH.HTML p (EditQuery model Unit)

-- | Used in edit/create forms
-- | TODO would be cool to derive instance of this for Generic
class EntityBuilder model where
   empty :: model
   -- | key -> value -> mod model fn
   setFieldValue :: String -> String -> model -> Either String model

-- | Common Query and Input for edit and create
data EditInput model = Retrieve (KeyT model) | Empty

data EditQuery model a
  = Set (EditInput model) a
   | SetVal String String a
   | Save a

-- | Misc
data Proxy a = Proxy

type EntityURI = String
