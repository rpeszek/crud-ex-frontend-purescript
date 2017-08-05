module CrudReuse.Common (
  AjaxM
 , class RestfullyGet
 , getEntities
 , class ViewableEntity
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
class RestfullyGet e a where
  getEntities :: e (Array (Entity(KeyT a) a)) 

 but I do not know how to do lambda level expressions in purescript (functional dependencies?)
 code like 
instance restGet :: RestfullyGet (AjaxM e) Thing where ...
does not compile
-}
class RestfullyGet e a where
  getEntities :: AjaxM e (Array (Entity(KeyT a) a)) 

--class EditableEntity e a where
--  createEntity :: 
--  updateEntity ::
--  deleteEntity ::

class ViewableEntity a where 
   detailedView :: forall p i. Entity (KeyT a) a -> HH.HTML p i
   listView :: forall p i. Entity (KeyT a) a -> HH.HTML p i
