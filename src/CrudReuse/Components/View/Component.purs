module CrudReuse.Components.View.Component (State, Query(..), ui, Slot(..)) where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import CrudReuse.Common (class EntityGET, class EntityReadHTML, class EntityRoute, AjaxM, Proxy(..), getEntities, getEntity, listView, readView)
import CrudReuse.Model (Entity(..), KeyT, unKey, toEntity)
import CrudReuse.Routing (CrudRoute(..), crudUri)
import Data.Either (Either(..), either)
--import Data.Maybe (Maybe(..))

type Input model = KeyT model

type State model =
  { loading :: Boolean
  , key :: KeyT model
  , errOrModel :: Either String model
  }

data Query model a
  = GetSingle (KeyT model) a

data Slot = Slot
derive instance eqListSlot :: Eq Slot
derive instance ordListSlot :: Ord Slot
  
initialState :: forall model . KeyT model -> State model
initialState i = { loading: false, key: i, errOrModel: Left "Not Retrieved" }

{-
  H.component does not receive initial call from runUI, this will be called from parent eventually
  https://github.com/slamdata/purescript-halogen/issues/444
-}
ui :: forall eff model. EntityReadHTML model => EntityGET eff model => EntityRoute model => Proxy model -> H.Component HH.HTML (Query model) (Input model) Void (AjaxM eff)
ui proxy =
  H.component
    { initialState: initialState
    , render : render
    , eval : eval
    , receiver: HE.input GetSingle
    }
  where
  render :: State model -> H.ComponentHTML (Query model)
  render st =
    HH.form_ $
      [ 
        HH.div_
          case st.errOrModel of
            Left err -> [
              HH.button
                  [ HP.disabled st.loading
                  , HE.onClick (HE.input_ $ GetSingle st.key)
                  ]
                  [ HH.text "Test Fetch" ]
            ]
            Right res ->
               let ent :: Entity (KeyT model) model
                   ent = Entity {id: st.key, entity: res}
               in [ HH.h2_
                   [ HH.text "Entity" ]
                  , HH.div_
                     [ 
                        readView ent
                      , HH.div_ [
                           HH.button [ 
                              HP.disabled st.loading
                            , HE.onClick (HE.input_ $ GetSingle st.key)
                           ]
                           [ HH.text "Refresh" ]
                          , HH.button [ 
                             HP.disabled st.loading
                            , HE.onClick (HE.input_ $ GetSingle st.key)
                           ]
                           [ HH.text "Delete" ]
                           , HH.a [ HP.href $ crudUri (ListR::CrudRoute model)] [ HH.text "Cancel"]
                        ]
                     ] 
                 ]
       , HH.p_
            [ HH.text (if st.loading then "Working..." else either id (const "") st.errOrModel) ]
      ]

  eval :: Query model ~> H.ComponentDSL (State model) (Query model) Void (AjaxM eff)
  eval = case _ of
    GetSingle key next -> do
      H.modify (_ { loading = true, key = key })
      errOrModel <- H.liftAff $ getEntity key
      H.modify (_ { loading = false, errOrModel = errOrModel })
      pure next
