module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Network.HTTP.Affjax as AX
--import Component (ui)
import CrudReuse.Components.Crud.Component as R
import Control.Monad.Aff (forkAff)
import CrudEx.Model
import CrudReuse.Common

--import CrudReuse.Components.List.Component as List
--import CrudEx.Model (Thing)


-- | Run the app.
main :: Eff (HA.HalogenEffects (ajax :: AX.AJAX)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  driver <- runUI (R.ui (Proxy::Proxy Thing)) unit body
  forkAff $ R.dispatch driver
