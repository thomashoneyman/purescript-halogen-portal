module Example.Component.Child where

import Prelude
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

data Action
  = HandleClick

data Query a
  = GetCount (Int -> a)

data Output
  = Clicked

type State
  = Int

component :: forall m. H.Component Query Unit Output m
component =
  H.mkComponent
    { initialState: const 0
    , render:
      \_ ->
        HH.div_
          [ HH.button
              [ HE.onClick \_ -> HandleClick ]
              [ HH.text "I'm the child." ]
          , HH.text "I'm rendered within the parent in the component tree, but elsewhere in the DOM."
          ]
    , eval:
      H.mkEval
        $ H.defaultEval
            { handleAction = handleAction
            , handleQuery = handleQuery
            }
    }
  where
  handleAction = case _ of
    HandleClick -> do
      H.modify_ (_ + 1)
      H.raise Clicked

  handleQuery :: forall a. Query a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery = case _ of
    GetCount reply -> do
      int <- H.get
      pure $ Just $ reply int
