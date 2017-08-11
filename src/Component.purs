module Component (State, Query(..), ui) where

{-
  Temporary testing, TODO remove this
-}
import Prelude
import CrudEx.Model.Thing
import CrudReuse.Server as Serv
import CrudReuse.Common (AjaxErrM)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax as AX
import Control.Monad.Aff (Aff)
import CrudReuse.Common (getEntities)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

type State =
  { loading :: Boolean
  , things :: Either String (Array ThingEntity)
  }

data Query a
  = MakeRequest a

{-
  H.component does not receive initial call from runUI, this will be called from parent eventually
  https://github.com/slamdata/purescript-halogen/issues/444
-}
ui :: forall eff. H.Component HH.HTML Query Unit Void (Aff (ajax :: AX.AJAX | eff))
ui =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing -- const $ Just $ MakeRequest unit
    }
  where

  initialState :: State
  initialState = { loading: false, things: Left "Not Retrieved" }

  render :: State -> H.ComponentHTML Query
  render st =
    HH.form_ $
      [ HH.h1_ [ HH.text "List of Entities" ]
      , HH.p_
          [ HH.text (if st.loading then "Working..." else "") ]
      , HH.div_
          case st.things of
            Left err -> [
              HH.button
                  [ HP.disabled st.loading
                  , HE.onClick (HE.input_ MakeRequest)
                  ]
                  [ HH.text "Test Fetch" ]
            ]
            Right res ->
              [ HH.h2_
                  [ HH.text "Response:" ]
              , HH.pre_
                  [ HH.code_ [ HH.text (show res) ] ]
              ]
      ]

  getThings :: forall e. AjaxErrM e (Array ThingEntity)
  getThings = Serv.getList "things"

  eval :: Query ~> H.ComponentDSL State Query Void (Aff (ajax :: AX.AJAX | eff))
  eval = case _ of
    MakeRequest next -> do
      H.modify (_ { loading = true })
      things <- H.liftAff $ getThings
      H.modify (_ { loading = false, things = things })
      pure next
