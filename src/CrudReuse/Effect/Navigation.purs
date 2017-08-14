module CrudReuse.Effect.Navigation where
  
import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Unsafe (unsafeCoerceAff)
import Control.Monad.Eff (Eff, kind Effect)

foreign import data NAVIGATION :: Effect

foreign import navigateTo :: forall e. String -> Eff (nav :: NAVIGATION | e) Unit

-- | this experiment is to fix compalation issues where liftAff did not work
-- it appears purscript has problem coercing Aff eff to Aff (nav :: NAVIGATION | eff) 
liftNav :: forall e a . Aff e a -> Aff (nav :: NAVIGATION | e) a
liftNav = unsafeCoerceAff  
   
