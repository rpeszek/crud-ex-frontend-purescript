module CrudReuse.Effect.Navigation where
  
import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Unsafe (unsafeCoerceAff)
import Control.Monad.Eff (Eff, kind Effect)

foreign import data NAVIGATION :: Effect

foreign import navigateTo :: forall e. String -> Eff (nav :: NAVIGATION | e) Unit

-- | it appears purscript has problem coercing Aff eff to Aff (nav :: NAVIGATION | eff) 
-- | this just treats computation with fewer effects as computation with additional 
-- | NAVIGATION effect and appears to be morally sound
liftAddNav :: forall e a . Aff e a -> Aff (nav :: NAVIGATION | e) a
liftAddNav = unsafeCoerceAff  
   
