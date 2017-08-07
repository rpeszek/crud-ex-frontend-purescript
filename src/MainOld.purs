module MainOld where

import Prelude
import Control.Monad.Eff (Eff)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Network.HTTP.Affjax as AX
--import Component (ui)
import CrudReuse.Components.List.Component as List
import CrudEx.Model (Thing)

initState :: List.State Thing
initState = List.initialState 

-- | Run the app.
mainOld :: Eff (HA.HalogenEffects (ajax :: AX.AJAX)) Unit
mainOld = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI (List.ui initState) unit body
