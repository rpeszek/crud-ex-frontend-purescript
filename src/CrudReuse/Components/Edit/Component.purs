module CrudReuse.Components.Edit.Component where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust, maybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import CrudReuse.Effect.Navigation (liftNav, navigateTo)
import CrudReuse.Routing (CrudRoute(..), crudUri)
import CrudReuse.Model (KeyT)
import CrudReuse.ReuseApi (class EntityBuilder, class EntityEditHTML, class EntityREST, class EntityRoute, AppM, EditInput(Empty, Retrieve), EditQuery(Set, Save, SetVal), Proxy, editView, empty, getEntity, postEntity, putEntity_, setFieldValue)

--import Data.Maybe (Maybe(..))

type Input model = EditInput model
type Query model = EditQuery model

type State model =
  { loading :: Boolean
  , maybeKey :: Maybe (KeyT model)
  , model :: model
  , maybeErrMsg :: Maybe String
  }


data CreateSlot = CreateSlot
derive instance eqCreateSlot :: Eq (CreateSlot)
derive instance ordCreateSlot :: Ord (CreateSlot)

data EditSlot = EditSlot
derive instance eqEditSlot :: Eq (EditSlot)
derive instance ordEditSlot :: Ord (EditSlot)
  
initialState :: forall model . EntityBuilder model => Input model -> State model
initialState inp = case inp of 
      Retrieve key -> { loading: true, maybeKey: Just key, model: empty, maybeErrMsg: Just "Not Retrieved" }
      Empty -> {loading: false, maybeKey: Nothing, model: empty, maybeErrMsg: Nothing}

test :: forall model p. HH.HTML p (Query model Unit)
test =          HH.div_ $
                      [ HH.label_
                           [ HH.div_ [ HH.text "name:" ]
                          , HH.input
                              [ HP.value "name"
                                , HE.onValueInput (HE.input $ SetVal "name")
                              ]
                           ]
                      ]

{-
  H.component does not receive initial call from runUI, this will be called from parent eventually
  https://github.com/slamdata/purescript-halogen/issues/444
-}
ui :: forall eff model. 
      Show model =>
      EntityEditHTML model => 
      EntityREST eff model => 
      EntityRoute model => 
      EntityBuilder model =>
      Proxy model -> H.Component HH.HTML (Query model) (Input model) Void (AppM eff)
ui proxy =
  H.component
    { initialState: initialState
    , render : render
    , eval : eval
    , receiver: HE.input $ Set
    }
  where
  
  render :: State model -> H.ComponentHTML (Query model)
  render st =
    HH.form_ $
      [ 
        HH.div_
          [ editView st.model]
          , HH.div_ [
              HH.button [ 
                  HP.disabled (st.loading || isJust st.maybeErrMsg)
                  , HE.onClick (HE.input_ $ Save)
              ][ HH.text "Save" ]
            , HH.a [ HP.href $ crudUri $ returnRoute st] [ HH.text "Cancel"]
          ]            
       , HH.p_ [ HH.text (if st.loading then "Working..." else maybe "" id st.maybeErrMsg) ]
      ]

  returnRoute :: State model -> CrudRoute model
  returnRoute st = case st.maybeKey of 
               Nothing -> ListR :: CrudRoute model
               Just key -> ViewR key

  eval :: Query model ~> H.ComponentDSL (State model) (Query model) Void (AppM eff)
  eval = case _ of
    Set (Retrieve key) next -> do
      H.modify (_ { loading = true, maybeKey = Just key })
      errOrModel <-  H.liftAff $ liftNav $ getEntity key
      case errOrModel of 
         Left msg -> H.modify (_ { loading = false, maybeErrMsg = Just msg })
         Right mdl -> H.modify (_ { loading = false, model = mdl, maybeErrMsg = Nothing })
      pure next
    Set Empty next -> do
      H.put $ initialState Empty
      pure next
    SetVal key val next -> do
         st <- H.get
         let newModelOrErr = setFieldValue key val st.model
         case newModelOrErr of 
            Right model -> 
               H.modify  (_ { model = model, maybeErrMsg = Nothing })
            Left msg -> 
               H.modify  (_ { maybeErrMsg = Just msg })
         pure next
    Save next -> do
          st <- H.get 
          case st.maybeErrMsg of 
             Just msg -> 
               H.modify (_ {maybeErrMsg = Just "Developer error: Inconsistent state"})
             Nothing -> do
               H.modify (_ {loading = true})
               resOrErr <- case st.maybeKey of 
                    Nothing -> H.liftAff $ liftNav $ postEntity st.model
                    Just key ->   H.liftAff $ liftNav $  putEntity_ key st.model
               case resOrErr of 
                    Left errMsg -> H.modify (_ {loading = false, maybeErrMsg = Just errMsg})
                    Right _ ->  H.liftEff $ navigateTo  $ crudUri $ returnRoute st
               pure unit
          pure next
