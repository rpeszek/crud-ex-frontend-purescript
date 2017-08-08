{-
  Temporary work on common parent component
  Ideally most of this logic should move to CrudReuse
-}
module CrudEx.Components.Router.Component where

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
import CrudEx.Routing (Routes(..), routing)
import CrudReuse.Common (AjaxM, Proxy(..))
import CrudReuse.Model (KeyT(..))
import Data.Const (Const(..))
import Data.Functor.Coproduct (Coproduct(..))
import Halogen.Component.ChildPath (ChildPath, cp1, cp2, cp3, cpR)
import Halogen.Data.Prism (type (\/), type (<\/>))
import Routing (matchesAff)
-- TODO needs common effect type (Aff (ajax :: AX.AJAX | eff))

data Query a
  = Dispatch Routes a

init :: State
init = { currentRoute: ThingList }

type State =
  { currentRoute :: Routes
  }

type ChildQuery = ListC.Query <\/> ViewC.Query <\/> MsgC.Query <\/> Const Void
type ChildSlot = ListC.Slot \/ ViewC.Slot \/ MsgC.Slot \/ Void

pathToList :: ChildPath ListC.Query ChildQuery ListC.Slot ChildSlot
pathToList = cp1

pathToView :: ChildPath ViewC.Query ChildQuery ViewC.Slot ChildSlot
pathToView = cp2

pathToMessage :: ChildPath MsgC.Query ChildQuery MsgC.Slot ChildSlot
pathToMessage = cp3

type QueryP
  = Coproduct Query ChildQuery

ui :: forall eff. H.Component HH.HTML Query Unit Void (AjaxM eff)
ui = H.parentComponent
  { initialState: const init
  , render
  , eval
  , receiver: const Nothing
  }
  where
    render :: State -> H.ParentHTML Query ChildQuery ChildSlot (AjaxM eff)
    render st =
      HH.div_
        [ HH.h1_ [ HH.text (show st.currentRoute) ] 
        , viewPage st.currentRoute
        ]
         
    viewPage :: Routes -> H.ParentHTML Query ChildQuery ChildSlot (AjaxM eff)
    viewPage ThingList =
      HH.slot' pathToList ListC.Slot (ListC.ui proxy) ListC.GetList absurd
    viewPage (ThingView (KeyT i)) = 
      HH.slot' pathToView ViewC.Slot (ViewC.ui proxy) i absurd       
    viewPage (NotDone msg) =
      HH.slot' pathToMessage MsgC.Slot MsgC.ui msg absurd
    -- TODO needs to pass message
    viewPage _ =
      HH.slot' pathToMessage MsgC.Slot MsgC.ui "Not Done" absurd

    eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void (AjaxM eff)
    eval (Dispatch routeEl next) = do
      modify (_ { currentRoute = routeEl })
      pure next
    
    proxy :: Proxy Thing
    proxy = Proxy 

dispatch :: forall eff. H.HalogenIO Query Void (Aff (HA.HalogenEffects eff))
            -> Aff (HA.HalogenEffects eff) Unit
dispatch driver = do
  Tuple old new <- matchesAff routing
  dispatchNewRoute driver old new

dispatchNewRoute :: forall eff. H.HalogenIO Query Void (Aff (HA.HalogenEffects eff))
          -> Maybe Routes
          -> Routes
          -> Aff (HA.HalogenEffects eff) Unit
dispatchNewRoute driver _ =
  driver.query <<< H.action <<< Dispatch
