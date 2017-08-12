module CrudReuse.Effect.Navigation where
  
import Prelude
import Control.Monad.Eff (Eff, kind Effect)
-- import CrudReuse.Debug (debugShow)

foreign import data NAVIGATION :: Effect

foreign import navigateTo :: forall e. String -> Eff (nav :: NAVIGATION | e) Unit
