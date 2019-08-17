module Example.Component.Parent where

import Prelude
import Data.Const (Const)
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
  = ( portal :: H.Slot (Const Void) Child.Output Unit
    )

component :: forall m. MonadAff m => H.Component HH.HTML (Const Void) Unit Void m
component =
  H.mkComponent
    { initialState: identity
    , render:
      \_ ->
        HH.div_
          [ HH.text "I'm the parent"
          , HH.slot (SProxy :: SProxy "portal") unit Portal.component
              { child: Child.component
              , input: unit
              , targetElement: Nothing
              }
              (Just <<< HandleChild)
          ]
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  handleAction = case _ of
    HandleChild output -> case output of
      Child.Clicked -> Console.log "clicked"
