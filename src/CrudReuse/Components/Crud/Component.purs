module CrudReuse.Components.Crud.Component where

import Prelude
import CrudReuse.Components.List.Component as ListC
import CrudReuse.Components.Message.Component as MsgC
import CrudReuse.Components.View.Component as ViewC
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Control.Monad.State.Class (modify)
import CrudReuse.Common (class EntityGET, class EntityReadHTML, class EntityRoute, AjaxM, Proxy)
import CrudReuse.Routing (CrudRoute(ViewR, ListR))
import Data.Const (Const)
import Halogen.Component.ChildPath (ChildPath, cp1, cp2, cp3)
import Halogen.Data.Prism (type (\/), type (<\/>))
import CrudReuse.Debug (debug)


data Input model = Input (CrudRoute model)

extractInput :: forall model . Input model -> CrudRoute model
extractInput (Input route) = route

data Slot model = Slot
derive instance eqListSlot ::  Eq (Slot a)
derive instance ordListSlot ::  Ord (Slot a)

data Query model a
  = Dispatch (CrudRoute model) a

init :: forall model . State model
init = { currentRoute: ListR }

type State model=
  { currentRoute :: CrudRoute model
  }

type ChildQuery model = ListC.Query <\/> ViewC.Query model <\/> MsgC.Query <\/> Const Void
type ChildSlot = ListC.Slot \/ ViewC.Slot \/ MsgC.Slot \/ Void

pathToList :: forall model. ChildPath ListC.Query (ChildQuery model) ListC.Slot ChildSlot
pathToList = cp1

pathToView :: forall model .ChildPath (ViewC.Query model) (ChildQuery model) ViewC.Slot ChildSlot
pathToView = cp2

pathToMessage :: forall model. ChildPath MsgC.Query (ChildQuery model) MsgC.Slot ChildSlot
pathToMessage = cp3

ui :: forall eff model. Show model => EntityReadHTML model => EntityGET eff model => EntityRoute model => Proxy model -> H.Component HH.HTML (Query model) (Input model) Void (AjaxM eff)
ui proxy = H.parentComponent
  { initialState: const init
  , render : render
  , eval : eval
  , receiver: HE.input (Dispatch <<< extractInput)
  }
  where
    render :: State model -> H.ParentHTML (Query model) (ChildQuery model) ChildSlot (AjaxM eff)
    render st =
      HH.div_
        [ HH.h1_ [ HH.text (show st.currentRoute) ] 
        , viewPage st.currentRoute
        ]
         
    viewPage :: CrudRoute model -> H.ParentHTML (Query model) (ChildQuery model) ChildSlot (AjaxM eff)
    viewPage ListR =
      HH.slot' pathToList ListC.Slot (ListC.ui proxy) ListC.GetList absurd
    viewPage (ViewR key) = 
      HH.slot' pathToView ViewC.Slot (ViewC.ui proxy) key absurd       
    viewPage _ =
      HH.slot' pathToMessage MsgC.Slot MsgC.ui "Not Done" absurd

    eval :: (Query model) ~> H.ParentDSL (State model) (Query model) (ChildQuery model) ChildSlot Void (AjaxM eff)
    eval (Dispatch routeEl next) = debug "crud eval" do
      modify (_ { currentRoute = routeEl })
      --  |Interestingly, the following H.query' s seemed not needed, adding similar code to parent of
      --   this component seems to do the trick
      _ <- case routeEl of 
              ListR    -> H.query' pathToList ListC.Slot (ListC.HandleInput ListC.GetList unit)
              ViewR k  -> H.query' pathToView ViewC.Slot (ViewC.GetSingle k unit)
              _        -> H.query' pathToMessage MsgC.Slot (MsgC.HandleInput "Not Done" unit)
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
