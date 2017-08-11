module CrudEx.Components.App.Component where
  
import Prelude
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import CrudReuse.Components.Crud.Component as CrudC
import CrudReuse.Components.Message.Component as MsgC
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Control.Monad.Aff (Aff)
import Control.Monad.State.Class (modify)
import CrudEx.Model.Other (Other)
import CrudEx.Model.Thing (Thing)
import CrudEx.Routing (AppRoute(..), appRoute, appUri)
import CrudReuse.Common (AjaxM, Proxy(Proxy))
import CrudReuse.Debug (debug)
import CrudReuse.Routing (CrudRoute(ListR))
import Data.Const (Const)
import Halogen.Component.ChildPath (ChildPath, cp1, cp2, cp3)
import Halogen.Data.Prism (type (\/), type (<\/>))
import Routing (matchesAff)

type Input = AppRoute

data Query a = Get AppRoute a 

type State = { currentAppR :: AppRoute }

type ChildQuery = CrudC.Query Thing <\/> CrudC.Query Other <\/> MsgC.Query <\/> Const Void
type ChildSlot = CrudC.Slot Thing \/ CrudC.Slot Other \/ MsgC.Slot \/ Void

pathToThing :: ChildPath (CrudC.Query Thing) ChildQuery (CrudC.Slot Thing) ChildSlot
pathToThing = cp1

pathToOther :: ChildPath (CrudC.Query Other) ChildQuery (CrudC.Slot Other) ChildSlot
pathToOther = cp2

pathToMessage :: ChildPath MsgC.Query ChildQuery MsgC.Slot ChildSlot
pathToMessage = cp3

ui :: forall eff. H.Component HH.HTML Query Input Void (AjaxM eff)
ui = H.parentComponent
  { initialState: const $ { currentAppR : MsgR "Start"}
  , render
  , eval
  , receiver: const Nothing
  }
  where
    render :: State -> H.ParentHTML Query ChildQuery ChildSlot (AjaxM eff)
    render st =
      HH.div_
        [ HH.h4_ [ HH.text ("Menu") ]
        , HH.ul_ (map link [
            ThingR ListR
            , OtherR ListR
            , MsgR "Hello World"
         ])
        , viewPage st.currentAppR
        ]

    link :: forall p i. AppRoute -> HH.HTML p i
    link r = HH.li_ [ HH.a [ HP.href $ appUri r ] [ HH.text $ appUri r ] ]

    viewPage :: AppRoute -> H.ParentHTML Query ChildQuery ChildSlot (AjaxM eff)
    viewPage (ThingR r) =
      HH.slot' pathToThing CrudC.Slot (CrudC.ui Proxy) (CrudC.Input r) absurd
    viewPage (OtherR r) =
      HH.slot' pathToOther CrudC.Slot (CrudC.ui Proxy) (CrudC.Input r) absurd
    viewPage (MsgR r) =
      HH.slot' pathToMessage MsgC.Slot MsgC.ui r absurd
    --viewPage _ =
    --  HH.div_ []

    eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void (AjaxM eff)
    eval (Get routeEl next) = debug "app eval" do
        modify (_ { currentAppR = routeEl })
        pure next


dispatch ::  forall eff. H.HalogenIO Query Void (Aff (HA.HalogenEffects eff))
            -> Aff (HA.HalogenEffects eff) Unit
dispatch driver = do
  Tuple old new <- matchesAff appRoute
  dispatchNewRoute driver old new

dispatchNewRoute :: forall eff. H.HalogenIO Query Void (Aff (HA.HalogenEffects eff))
          -> Maybe AppRoute
          -> AppRoute
          -> Aff (HA.HalogenEffects eff) Unit
dispatchNewRoute driver _ =
  driver.query <<< H.action <<< Get
