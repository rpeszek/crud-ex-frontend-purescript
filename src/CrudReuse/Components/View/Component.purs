module CrudReuse.Components.View.Component (State, Query(..), ui, Slot(..)) where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import CrudReuse.Common (class EntityREST, class EntityReadHTML, class EntityRoute, AppM, Proxy(..), deleteEntity, getEntities, getEntity, listView, readView)
import CrudReuse.Effect.Navigation (navigateTo)
import CrudReuse.Model (Entity(..), KeyT, unKey, toEntity)
import CrudReuse.Routing (CrudRoute(..), crudUri)
import DOM.Event.KeyboardEvent (key)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), maybe)
--import Data.Maybe (Maybe(..))

type Input model = KeyT model

type State model =
  { loading :: Boolean
  , key :: KeyT model
  , maybeModel :: Maybe model
  , maybeMsg :: Maybe String
  }

data Query model a
  = GetSingle (KeyT model) a
    | Delete (KeyT model) a

data Slot = Slot
derive instance eqListSlot :: Eq Slot
derive instance ordListSlot :: Ord Slot
  
initialState :: forall model . KeyT model -> State model
initialState i = { loading: false, key: i, maybeModel: Nothing, maybeMsg: Just "Not Retrieved" }

{-
  H.component does not receive initial call from runUI, this will be called from parent eventually
  https://github.com/slamdata/purescript-halogen/issues/444
-}
ui :: forall eff model. EntityReadHTML model => EntityREST eff model => EntityRoute model => Proxy model -> H.Component HH.HTML (Query model) (Input model) Void (AppM eff)
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
          case st.maybeModel of
            Nothing -> [
              HH.button
                  [ HE.onClick (HE.input_ $ GetSingle st.key)]
                  [ HH.text "Test Fetch" ]
            ]
            Just res ->
               let ent :: Entity (KeyT model) model
                   ent = Entity {id: st.key, entity: res}
               in [ HH.div_
                     [ readView ent
                      , HH.div_ [
                           HH.button [ 
                              HP.disabled st.loading
                            , HE.onClick (HE.input_ $ GetSingle st.key)
                           ]
                           [ HH.text "Refresh" ]
                          , HH.button [ 
                             HP.disabled st.loading
                            , HE.onClick (HE.input_ $ Delete st.key)
                           ]
                           [ HH.text "Delete" ]
                           , HH.a [ HP.href $ crudUri (ListR::CrudRoute model)] [ HH.text "Cancel"]
                        ]
                     ] 
                 ]
       , HH.p_ [ HH.text (if st.loading then "Working..." else maybe "" id st.maybeMsg) ]
      ]

  eval :: Query model ~> H.ComponentDSL (State model) (Query model) Void (AppM eff)
  eval = case _ of
    GetSingle key next -> do
      H.modify (_ { loading = true, key = key })
      errOrModel <- H.liftAff $ getEntity key
      case errOrModel of 
         Left msg -> H.modify (_ { loading = false, maybeMsg = Just msg })
         Right mdl -> H.modify (_ { loading = false, maybeModel = Just mdl })
      pure next
    Delete key next -> do
      res <- H.liftAff $ deleteEntity key
      case res of 
          Left msg -> H.modify (_ { loading = false, maybeMsg = Just msg }) 
          Right _  ->  H.liftEff $ navigateTo $ crudUri (ListR::CrudRoute model)
      pure next
