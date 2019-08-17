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
  = ( portal :: H.Slot Child.Query Child.Output Unit
    )

component :: forall m. MonadAff m => H.Component HH.HTML (Const Void) Unit Void m
component =
  H.mkComponent
    { initialState: identity
    , render:
      \_ ->
        HH.div
          []
          [ HH.text "I'm the parent"
          , HH.slot portal unit Portal.component
              { child: Child.component
              , input: unit
              , targetElement: Nothing
              }
              (Just <<< HandleChild)
          ]
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  portal :: SProxy "portal"
  portal = SProxy

  handleAction = case _ of
    HandleChild output -> case output of
      -- We can receive messages directly from the child, as usual
      Child.Clicked -> do
        Console.log "clicked"
        -- and we can also query the child directly, as usual
        traverse_ Console.logShow =<< H.query portal unit (H.request Child.GetCount)
