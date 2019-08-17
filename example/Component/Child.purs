module Example.Component.Child where

import Prelude
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

data Action
  = HandleClick

data Output
  = Clicked

component :: forall m. H.Component HH.HTML (Const Void) Unit Output m
component =
  H.mkComponent
    { initialState: identity
    , render:
      \_ ->
        HH.div
          [ HE.onClick \_ -> Just HandleClick ]
          [ HH.text "I'm the child. I'm rendered inside the parent logically, but outside the parent in the HTML." ]
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  handleAction = case _ of
    HandleClick -> H.raise Clicked
