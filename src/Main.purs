module Main where

import Prelude
import CrudReuse.Common
import CrudEx.Components.App.Component as App
import Halogen.Aff as HA
import Network.HTTP.Affjax as AX
import Control.Monad.Aff (forkAff)
import Control.Monad.Eff (Eff)
import CrudEx.Routing (AppRoute(..))
import CrudReuse.Routing (CrudRoutes(List))
import Halogen.VDom.Driver (runUI)

--import CrudReuse.Components.List.Component as List
--import CrudEx.Model.Thing (Thing)


-- | Run the app.
main :: Eff (HA.HalogenEffects (ajax :: AX.AJAX)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  driver <- runUI App.ui (ThingR List) body
  forkAff $ App.dispatch driver
