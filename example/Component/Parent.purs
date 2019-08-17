module Example.Component.Parent where

import Prelude
import Data.Const (Const)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console as Console
import Example.Component.Child as Child
import Halogen as H
import Halogen.HTML as HH
import Halogen.Portal as Portal

data Action
  = HandleChild Child.Output

type ChildSlots
  = ( child :: H.Slot Child.Query Child.Output Unit
    )

_child :: SProxy "child"
_child = SProxy

component :: forall m. MonadAff m => H.Component HH.HTML (Const Void) Unit Void m
component =
  H.mkComponent
    { initialState: identity
    , render:
      \_ ->
        HH.div
          []
          [ HH.text "I'm the parent"
          -- This is almost identical to using the `slot` function, but this component
          -- will _not_ be rendered within the parent component <div> in the DOM.
          , Portal.portal _child unit Child.component unit Nothing (Just <<< HandleChild)
          ]
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  handleAction = case _ of
    HandleChild output -> case output of
      -- We can receive messages directly from the child, as usual
      Child.Clicked -> do
        Console.log "clicked"
        -- and we can also query the child directly, as usual
        traverse_ Console.logShow =<< H.query _child unit (H.request Child.GetCount)
