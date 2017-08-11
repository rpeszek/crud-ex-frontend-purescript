module Main where

import Prelude
import CrudEx.Components.App.Component as App
import Halogen.Aff as HA
import Network.HTTP.Affjax as AX
import Control.Monad.Aff (forkAff)
import Control.Monad.Eff (Eff)
import CrudEx.Routing (AppRoute(..))
import CrudReuse.Routing (CrudRoute(ListR))
import Halogen.VDom.Driver (runUI)

-- | Run the app.
main :: Eff (HA.HalogenEffects (ajax :: AX.AJAX)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  driver <- runUI App.ui (ThingR ListR) body
  forkAff $ App.dispatch driver
