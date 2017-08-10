{-
  Temporary work on common parent component
  Ideally most of this logic should move to CrudReuse
-}
module CrudReuse.Components.Crud.Component where

import Prelude
import Data.Maybe
import Data.Tuple
import CrudEx.Model
import CrudReuse.Components.List.Component as ListC
import CrudReuse.Components.Message.Component as MsgC
import CrudReuse.Components.View.Component as ViewC
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Control.Monad.Aff (Aff)
import Control.Monad.State.Class (modify)
import CrudReuse.Common (class EntityGET, class EntityReadHTML, class EntityRoute, AjaxM, Proxy(..))
import CrudReuse.Model (KeyT(..))
import CrudReuse.Routing (CrudRoutes(..), routing)
import Data.Const (Const(..))
import Data.Functor.Coproduct (Coproduct(..))
import Halogen.Component.ChildPath (ChildPath, cp1, cp2, cp3, cpR)
import Halogen.Data.Prism (type (\/), type (<\/>))
import Routing (matchesAff)
-- TODO needs common effect type (Aff (ajax :: AX.AJAX | eff))

data Query model a
  = Dispatch (CrudRoutes model) a

init :: forall model . State model
init = { currentRoute: List }

type State model=
  { currentRoute :: CrudRoutes model
  }

type ChildQuery model = ListC.Query <\/> ViewC.Query model <\/> MsgC.Query <\/> Const Void
type ChildSlot = ListC.Slot \/ ViewC.Slot \/ MsgC.Slot \/ Void

pathToList :: forall model. ChildPath ListC.Query (ChildQuery model) ListC.Slot ChildSlot
pathToList = cp1

pathToView :: forall model .ChildPath (ViewC.Query model) (ChildQuery model) ViewC.Slot ChildSlot
pathToView = cp2

pathToMessage :: forall model. ChildPath MsgC.Query (ChildQuery model) MsgC.Slot ChildSlot
pathToMessage = cp3

type QueryP model
  = Coproduct (Query model) (ChildQuery model)

ui :: forall eff model. EntityReadHTML model => EntityGET eff model => EntityRoute model => Proxy model -> H.Component HH.HTML (Query model) Unit Void (AjaxM eff)
ui proxy = H.parentComponent
  { initialState: const init
  , render
  , eval
  , receiver: const Nothing
  }
  where
    render :: State model -> H.ParentHTML (Query model) (ChildQuery model) ChildSlot (AjaxM eff)
    render st =
      HH.div_
        [ HH.h1_ [ HH.text (show st.currentRoute) ] 
        , viewPage st.currentRoute
        ]
         
    viewPage :: CrudRoutes model -> H.ParentHTML (Query model) (ChildQuery model) ChildSlot (AjaxM eff)
    viewPage List =
      HH.slot' pathToList ListC.Slot (ListC.ui proxy) ListC.GetList absurd
    viewPage (View key) = 
      HH.slot' pathToView ViewC.Slot (ViewC.ui proxy) key absurd       
    viewPage (NotDone msg) =
      HH.slot' pathToMessage MsgC.Slot MsgC.ui msg absurd
    -- TODO needs to pass message
    viewPage _ =
      HH.slot' pathToMessage MsgC.Slot MsgC.ui "Not Done" absurd

    eval :: (Query model) ~> H.ParentDSL (State model) (Query model) (ChildQuery model) ChildSlot Void (AjaxM eff)
    eval (Dispatch routeEl next) = do
      modify (_ { currentRoute = routeEl })
      pure next
    

dispatch :: forall eff model. EntityRoute model => H.HalogenIO (Query model) Void (Aff (HA.HalogenEffects eff))
            -> Aff (HA.HalogenEffects eff) Unit
dispatch driver = do
  Tuple old new <- matchesAff routing
  dispatchNewRoute driver old new

dispatchNewRoute :: forall eff model. H.HalogenIO (Query model) Void (Aff (HA.HalogenEffects eff))
          -> Maybe (CrudRoutes model)
          -> CrudRoutes model
          -> Aff (HA.HalogenEffects eff) Unit
dispatchNewRoute driver _ =
  driver.query <<< H.action <<< Dispatch
