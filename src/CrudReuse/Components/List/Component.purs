module CrudReuse.Components.List.Component (State, Query(..), ui, Slot(..), Input(..)) where

import Prelude
import Data.Either (Either(..), either)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import CrudReuse.Effect.Navigation (liftAddNav)
import CrudReuse.Model (Entity, KeyT)
import CrudReuse.ReuseApi (class EntityGET, getEntities, listView, class EntityReadHTML, class EntityRoute, AppM, Proxy)
import CrudReuse.Routing (CrudRoute(..), crudUri)

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
ui :: forall eff model. 
              Show model => 
              EntityRoute model => 
              EntityReadHTML model => 
              EntityGET eff model => 
              Proxy model -> H.Component HH.HTML Query Input Void (AppM eff)
ui proxy =
  H.component
    { initialState: const initialState
    , render : render
    , eval : eval
    , receiver: {- debug "list receiver" $ -} HE.input HandleInput
    }
  where  
  render ::  State model -> H.ComponentHTML Query
  render st =
    HH.div_ $
      [ 
        HH.div_
          case st.errOrEntities of
            Left err -> [
              HH.button
                  [ HE.onClick (HE.input_ $ HandleInput GetList)
                  ]
                  [ HH.text "Test Fetch" ]
            ]
            Right res ->
              [ HH.div_ $
                   map listView res 
              ]
       , HH.p_ [HH.a [ HP.href $ crudUri (CreateR :: CrudRoute model)] [ HH.text "Create"]]
       , HH.p_
            [ HH.text (if st.loading then "Working..." else either id (const "") st.errOrEntities) ]
      ]

  eval :: Query ~> H.ComponentDSL (State model) Query Void (AppM eff)
  eval = case _ of
    HandleInput _ next -> {- debug "list eval" -} do
      H.modify (_ { loading = true })
      errOrEntities <- H.liftAff $ liftAddNav $ getEntities
      H.modify (_ { loading = false, errOrEntities = errOrEntities })
      pure next
