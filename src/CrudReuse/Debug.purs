module CrudReuse.Debug where
  
{-
 Elm like util to be able to console log pure code
-}

import Prelude
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Unsafe (unsafePerformEff)

debug :: forall a . String -> a -> a
debug s a = 
        let _ = unsafePerformEff $ log s
        in a

debugShow :: forall a . String -> Show a => a -> a
debugShow s a = debug (s <> " " <> show a) a
