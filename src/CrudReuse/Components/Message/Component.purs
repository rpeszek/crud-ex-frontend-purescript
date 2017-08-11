module CrudReuse.Components.Message.Component where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type Input = String

type State = String

data Query a = HandleInput String a

data Slot = Slot
derive instance eqMsgSlot :: Eq Slot
derive instance ordMsgSlot :: Ord Slot

ui :: forall m. H.Component HH.HTML Query Input Void m
ui =
  H.component
    { initialState: const initialState
    , render : render
    , eval : eval
    , receiver: HE.input HandleInput
    }
  where

  initialState :: State
  initialState = ""

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div_
      [ HH.text state
      ]

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    HandleInput n next -> do
      oldN <- H.get
      when (oldN /= n) $ H.put n
      pure next
