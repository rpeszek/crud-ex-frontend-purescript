{-Temporary work on common parent component-}
module CrudEx.Router where

import Prelude
import Data.Maybe
import Data.Tuple
import CrudEx.Model
import CrudReuse.Common (AjaxM)
import CrudReuse.Components.List.Component as ListC
import CrudReuse.Components.Message.Component as MsgC
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Control.Alt ((<|>))
import Control.Category (id)
import Control.Monad.Aff (Aff)
import Control.Monad.State.Class (modify)
import CrudReuse.Model (KeyT(..))
import Data.Either (Either)
import Data.Functor.Coproduct (Coproduct)
import Data.Generic (class Generic, gShow)
import Data.Int (floor)
import Data.String (toLower)
import Halogen.Component.ChildPath (ChildPath, cpR, cpL)
import Routing (matchesAff)
import Routing.Match (Match)
import Routing.Match.Class (lit, num, int, str)
import Network.HTTP.Affjax as AX
import Control.Monad.Aff (Aff)

-- TODO needs common effect type (Aff (ajax :: AX.AJAX | eff))

data Query a
  = Dispatch Routes a

data Routes
  = ThingList
   | ThingView (KeyT Thing)
   | ThingEdit (KeyT Thing)
   | NotDone String

instance showRoutes :: Show Routes where
  show ThingList = "List Things"
  show (ThingView (KeyT id)) = "View Thing" <> show id
  show (ThingEdit (KeyT id)) = "Edit Thing" <> show id
  show (NotDone _) = "Message"

init :: State
init = { currentRoute: ThingList }

routing :: Match Routes
routing = thingList
      <|> thingView
      <|> thingEdit
      <|> notDone
  where
    thingList = ThingList <$ oneSlash
    thingView = ThingView <$> (homeSlash *> lit "thingView" *> thingKey)
    thingEdit = ThingEdit <$> (homeSlash *> lit "thingEdit" *> thingKey)
    notDone = NotDone <$> (homeSlash *> lit "notDone" *> str)
    oneSlash :: Match Unit
    oneSlash = lit "/"
    homeSlash :: Match Unit
    homeSlash = lit ""
    --int :: Match Int
    --int = floor <$> num
    thingKey :: Match (KeyT Thing)
    thingKey = KeyT <$> int

type State =
  { currentRoute :: Routes
  }

type ChildQuery = Coproduct ListC.Query MsgC.Query
type ChildSlot = Either ListC.Slot MsgC.Slot

pathToList :: ChildPath ListC.Query ChildQuery ListC.Slot ChildSlot
pathToList = cpL

pathToMessage :: ChildPath MsgC.Query ChildQuery MsgC.Slot ChildSlot
pathToMessage = cpR

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
      HH.slot' pathToList ListC.Slot (ListC.ui initListState) unit absurd
        where 
         initListState :: ListC.State Thing
         initListState = ListC.initialState 
    viewPage (NotDone msg) =
      HH.slot' pathToMessage MsgC.Slot MsgC.ui msg absurd
    -- TODO needs to pass message
    viewPage _ =
      HH.slot' pathToMessage MsgC.Slot MsgC.ui "Not Done" absurd

    eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void (AjaxM eff)
    eval (Dispatch routeEl next) = do
      modify (_ { currentRoute = routeEl })
      pure next

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
