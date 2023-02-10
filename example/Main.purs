module Main where

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Example.Component.Parent as Parent
import Foreign.Object as Object
import Halogen.Aff as HA
import Halogen.Storybook (Stories, proxy, runStorybook)
import Prelude

stories :: forall m. MonadAff m => Stories m
stories =
  Object.fromFoldable
    [ Tuple "basic" $ proxy Parent.component ]

main :: Effect Unit
main =
  HA.runHalogenAff do
    runStorybook { stories, logo: Nothing } =<< HA.awaitBody
