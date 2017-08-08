module CrudReuse.Components.View.Component (State, Query(..), ui, Slot(..)) where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import CrudReuse.Common (class EntityGET, class EntityReadHTML, AjaxM, Proxy(..), readView, getEntities, getEntity, listView, listUri)
import CrudReuse.Model (Entity(..), KeyT(..), unKey, toEntity)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))

type Input = Int

type State model =
  { loading :: Boolean
  , key :: KeyT model
  , errOrModel :: Either String model
  }

data Query a
  = GetSingle Int a

data Slot = Slot
derive instance eqListSlot :: Eq Slot
derive instance ordListSlot :: Ord Slot
  
initialState :: forall model . Int -> State model
initialState i = { loading: false, key: KeyT i, errOrModel: Left "Not Retrieved" }

{-
  H.component does not receive initial call from runUI, this will be called from parent eventually
  https://github.com/slamdata/purescript-halogen/issues/444
-}
ui :: forall eff model. EntityReadHTML model => EntityGET eff model => Proxy model -> H.Component HH.HTML Query Input Void (AjaxM eff)
ui proxy =
  H.component
    { initialState: initState
    , render 
    , eval
    , receiver: HE.input GetSingle
    }
  where
  initState :: Int -> State model
  initState = initialState

  render :: State model -> H.ComponentHTML Query
  render st =
    HH.form_ $
      [ 
        HH.div_
          case st.errOrModel of
            Left err -> [
              HH.button
                  [ HP.disabled st.loading
                  , HE.onClick (HE.input_ $ GetSingle $ unKey st.key)
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
                            , HE.onClick (HE.input_ $ GetSingle $ unKey st.key)
                           ]
                           [ HH.text "Refresh" ]
                          , HH.button [ 
                             HP.disabled st.loading
                            , HE.onClick (HE.input_ $ GetSingle $ unKey st.key)
                           ]
                           [ HH.text "Delete" ]
                           , HH.a [ HP.href $ listUri proxy ] [ HH.text "Cancel"]
                        ]
                     ] 
                 ]
       , HH.p_
            [ HH.text (if st.loading then "Working..." else either id (const "") st.errOrModel) ]
      ]

  eval :: Query ~> H.ComponentDSL (State model) Query Void (AjaxM eff)
  eval = case _ of
    GetSingle i next -> do
      H.modify (_ { loading = true, key = KeyT i })
      oldS <- H.get
      errOrModel <- H.liftAff $ getEntity oldS.key
      H.modify (_ { loading = false, errOrModel = errOrModel })
      pure next
