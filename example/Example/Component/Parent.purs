module Example.Component.Parent where

import Prelude
import Data.Const (Const)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console as Console
import Example.Component.Child as Child
import Halogen as H
import Halogen.HTML as HH
import Halogen.Portal as Portal
import Type.Proxy (Proxy(..))

data Action
  = HandleChild Child.Output

type ChildSlots
  = ( child :: H.Slot Child.Query Child.Output Unit
    )

_child :: Proxy "child"
_child = Proxy

component :: forall m. MonadAff m => H.Component (Const Void) Unit Void m
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
          , Portal.portalAff _child unit Child.component unit Nothing (HandleChild)
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
        traverse_ Console.logShow =<< H.request _child unit Child.GetCount
