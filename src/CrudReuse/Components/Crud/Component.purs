module CrudReuse.Components.Crud.Component where

import Prelude
import CrudReuse.Components.Edit.Component as EditC
import CrudReuse.Components.List.Component as ListC
import CrudReuse.Components.View.Component as ViewC
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Control.Monad.State.Class (modify)
import CrudReuse.ReuseApi (class EntityBuilder, class EntityEditHTML, class EntityREST, class EntityReadHTML, class EntityRoute, AppM, EditInput(..), EditQuery(..), Proxy)
import CrudReuse.Debug (debug)
import CrudReuse.Routing (CrudRoute(..))
import Data.Const (Const)
import Halogen.Component.ChildPath (ChildPath, cp1, cp2, cp3, cp4)
import Halogen.Data.Prism (type (\/), type (<\/>))


data Input model = Input (CrudRoute model)

extractInput :: forall model . Input model -> CrudRoute model
extractInput (Input route) = route

data Slot model = Slot
derive instance eqCrudSlot ::  Eq (Slot a)
derive instance ordCrudSlot ::  Ord (Slot a)

data Query model a
  = Dispatch (CrudRoute model) a

init :: forall model . State model
init = { currentRoute: ListR }

type State model=
  { currentRoute :: CrudRoute model
  }

type ChildQuery model 
         = ListC.Query 
         <\/> ViewC.Query model 
         <\/> EditC.Query model 
         <\/> EditC.Query model 
         <\/> Const Void

type ChildSlot 
         = ListC.Slot 
         \/ ViewC.Slot 
         \/ EditC.CreateSlot 
         \/ EditC.EditSlot 
         \/ Void

pathToList :: forall model. ChildPath ListC.Query (ChildQuery model) ListC.Slot ChildSlot
pathToList = cp1

pathToView :: forall model .ChildPath (ViewC.Query model) (ChildQuery model) ViewC.Slot ChildSlot
pathToView = cp2

pathToCreate :: forall model .ChildPath (EditC.Query model) (ChildQuery model) (EditC.CreateSlot) ChildSlot
pathToCreate = cp3

pathToEdit :: forall model .ChildPath (EditC.Query model) (ChildQuery model) (EditC.EditSlot) ChildSlot
pathToEdit = cp4

ui :: forall eff model. 
          Show model => 
          EntityReadHTML model => 
          EntityEditHTML model => 
          EntityREST eff model => 
          EntityRoute model => 
          EntityBuilder model =>
          Proxy model -> 
          H.Component HH.HTML (Query model) (Input model) Void (AppM eff)
ui proxy = H.parentComponent
  { initialState: const init
  , render : render
  , eval : eval
  , receiver: HE.input (Dispatch <<< extractInput)
  }
  where
    render :: State model -> H.ParentHTML (Query model) (ChildQuery model) ChildSlot (AppM eff)
    render st =
      HH.div_
        [ HH.h1_ [ HH.text (show st.currentRoute) ] 
        , viewPage st.currentRoute
        ]
         
    viewPage :: CrudRoute model -> H.ParentHTML (Query model) (ChildQuery model) ChildSlot (AppM eff)
    viewPage ListR =
      HH.slot' pathToList ListC.Slot (ListC.ui proxy) ListC.GetList absurd
    viewPage (ViewR key) = 
      HH.slot' pathToView ViewC.Slot (ViewC.ui proxy) key absurd       
    viewPage CreateR =
      HH.slot' pathToCreate EditC.CreateSlot (EditC.ui proxy) Empty absurd
    viewPage (EditR key) =
      HH.slot' pathToEdit EditC.EditSlot (EditC.ui proxy) (Retrieve key) absurd

    eval :: (Query model) ~> H.ParentDSL (State model) (Query model) (ChildQuery model) ChildSlot Void (AppM eff)
    eval (Dispatch routeEl next) = debug "crud eval" do
      modify (_ { currentRoute = routeEl })
      --  |Interestingly, the following H.query' s seemed not needed, adding similar code to parent of
      --   this component seems to do the trick
      _ <- case routeEl of 
              ListR    -> H.query' pathToList ListC.Slot (ListC.HandleInput ListC.GetList unit)
              ViewR k  -> H.query' pathToView ViewC.Slot (ViewC.GetSingle k unit)
              CreateR  -> H.query' pathToCreate EditC.CreateSlot (Set Empty unit)
              EditR k  -> H.query' pathToEdit EditC.EditSlot (Set (Retrieve k) unit)
      pure next
    

---------
-- usefull for isolated testing of single crud, not used
---------
--
-- dispatch :: forall eff model. EntityRoute model => H.HalogenIO (Query model) Void (Aff (HA.HalogenEffects eff))
--             -> Aff (HA.HalogenEffects eff) Unit
-- dispatch driver = do
--   Tuple old new <- matchesAff crudRoute
--   dispatchNewRoute driver old new
-- 
-- dispatchNewRoute :: forall eff model. H.HalogenIO (Query model) Void (Aff (HA.HalogenEffects eff))
--           -> Maybe (CrudRoute model)
--           -> CrudRoute model
--           -> Aff (HA.HalogenEffects eff) Unit
-- dispatchNewRoute driver _ =
--   driver.query <<< H.action <<< Dispatch
