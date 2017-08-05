module CrudReuse.Components.List.Component (State, Query(..), ui, initialState) where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax as AX
import Control.Monad.Aff (Aff)
import CrudReuse.Common (class EntityGET, class EntityReadHTML, getEntities, listView)
import CrudReuse.Model (Entity, KeyT)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))

type State model =
  { loading :: Boolean
  , errOrEntities :: Either String (Array (Entity (KeyT model) model))
  }


data Query a
  = GetList a
  
initialState :: forall model . State model
initialState = { loading: false, errOrEntities: Left "Not Retrieved" }

{-
  H.component does not receive initial call from runUI, this will be called from parent eventually
  https://github.com/slamdata/purescript-halogen/issues/444
-}
ui :: forall eff model. EntityReadHTML model => EntityGET eff model => State model -> H.Component HH.HTML Query Unit Void (Aff (ajax :: AX.AJAX | eff))
ui initState =
  H.component
    { initialState: const initState
    , render
    , eval
    , receiver: const Nothing -- const $ Just $ GetList unit
    }
  where

  render ::  State model -> H.ComponentHTML Query
  render st =
    HH.form_ $
      [ 
        HH.div_
          case st.errOrEntities of
            Left err -> [
              HH.button
                  [ HP.disabled st.loading
                  , HE.onClick (HE.input_ GetList)
                  ]
                  [ HH.text "Test Fetch" ]
            ]
            Right res ->
              [ HH.h2_
                  [ HH.text "List" ]
              , HH.div_
                  [ HH.code_ $ map listView res ]
              ]
       , HH.p_
            [ HH.text (if st.loading then "Working..." else either id (const "") st.errOrEntities) ]
      ]

  eval :: Query ~> H.ComponentDSL (State model) Query Void (Aff (ajax :: AX.AJAX | eff))
  eval = case _ of
    GetList next -> do
      H.modify (_ { loading = true })
      errOrEntities <- H.liftAff $ getEntities
      H.modify (_ { loading = false, errOrEntities = errOrEntities })
      pure next
