module MainOld where

import Prelude
import CrudReuse.Components.List.Component as List
import Halogen.Aff as HA
import Network.HTTP.Affjax as AX
import Control.Monad.Eff (Eff)
import CrudEx.Model (Thing)
import CrudReuse.Common (Proxy(..))
import Halogen.VDom.Driver (runUI)

proxy :: Proxy Thing
proxy = Proxy 

-- | Run the app.
mainOld :: Eff (HA.HalogenEffects (ajax :: AX.AJAX)) Unit
mainOld = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI (List.ui proxy) List.GetList body
