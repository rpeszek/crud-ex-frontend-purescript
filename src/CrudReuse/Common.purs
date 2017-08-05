module CrudReuse.Common (
  AjaxM
 , class EntityGET
 , getEntities
 , getEntity
 , class EntityReadHTML
 , detailedView
 , listView
) where

import Prelude
import CrudReuse.Model
import Halogen.HTML as HH
import CrudReuse.Model (Entity(..))
import Control.Monad.Aff (Aff)
import Data.Either (Either)
import Network.HTTP.Affjax (AJAX)

type AjaxM e a = Aff (ajax :: AJAX | e) (Either String a)
  
{-
 It would be cleaner if I could define this for arbitrary monad effect:
class EntityGET e a where
  getEntities :: e (Array (Entity(KeyT a) a)) 

 but I do not know how to do lambda level expressions in purescript (functional dependencies?)
 code like 
instance restGet :: EntityGET (AjaxM e) Thing where ...
does not compile
-}
class EntityGET e a where
  getEntities :: AjaxM e (Array (Entity(KeyT a) a)) 
  getEntity :: KeyT a -> AjaxM e a 

class (EntityGET e a) <= EntityREST e a where
  postEntity :: a -> AjaxM e (Entity(KeyT a) a)
  putEntity :: KeyT a -> a -> AjaxM e a
  deleteEntity :: KeyT a -> AjaxM e Unit

class EntityReadHTML a where 
   detailedView :: forall p i. Entity (KeyT a) a -> HH.HTML p i
   listView :: forall p i. Entity (KeyT a) a -> HH.HTML p i

class EntityEditHTML a where 
   editView :: forall p i. Entity (KeyT a) a -> HH.HTML p i
