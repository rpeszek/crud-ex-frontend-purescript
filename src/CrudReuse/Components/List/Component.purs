module CrudReuse.Components.List.Component (State, Query(..), ui, Slot(..), Input(..)) where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import CrudReuse.Common (class EntityGET, class EntityReadHTML, AjaxM, Proxy(..), getEntities, listView)
import CrudReuse.Model (Entity, KeyT)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))

type State model =
  { loading :: Boolean
  , errOrEntities :: Either String (Array (Entity (KeyT model) model))
  }

data Input = GetList

data Query a = HandleInput Input a

data Slot = Slot
derive instance eqListSlot :: Eq Slot
derive instance ordListSlot :: Ord Slot
  
initialState :: forall model . State model
initialState = { loading: false, errOrEntities: Left "Not Retrieved" }

{-
  H.component does not receive initial call from runUI, this will be called from parent eventually
  https://github.com/slamdata/purescript-halogen/issues/444
-}
ui :: forall eff model. EntityReadHTML model => EntityGET eff model => Proxy model -> H.Component HH.HTML Query Input Void (AjaxM eff)
ui proxy =
  H.component
    { initialState: const initState
    , render
    , eval
    , receiver: HE.input HandleInput
    }
  where
  initState :: State model
  initState = initialState

  render ::  State model -> H.ComponentHTML Query
  render st =
    HH.form_ $
      [ 
        HH.div_
          case st.errOrEntities of
            Left err -> [
              HH.button
                  [ HP.disabled st.loading
                  , HE.onClick (HE.input_ $ HandleInput GetList)
                  ]
                  [ HH.text "Test Fetch" ]
            ]
            Right res ->
              [ HH.h2_
                  [ HH.text "List" ]
              , HH.div_ $
                   map listView res 
              ]
       , HH.p_
            [ HH.text (if st.loading then "Working..." else either id (const "") st.errOrEntities) ]
      ]

  eval :: Query ~> H.ComponentDSL (State model) Query Void (AjaxM eff)
  eval = case _ of
    HandleInput _ next -> do
      H.modify (_ { loading = true })
      errOrEntities <- H.liftAff $ getEntities
      H.modify (_ { loading = false, errOrEntities = errOrEntities })
      pure next
