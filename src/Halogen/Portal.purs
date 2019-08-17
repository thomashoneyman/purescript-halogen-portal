-- | A container component which renders a sub-tree to a DOM node not in the
-- | tree. This is useful for when a child component needs to 'break out' of a
-- | parent, like dialogs, modals, and tooltips, especially if the parent has
-- | z-indexing or overflow: hidden set.
module Halogen.Portal where

import Prelude
import Control.Coroutine (consumer)
import Control.Monad.Rec.Class (forever)
import Data.Coyoneda (unCoyoneda)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), maybe, maybe')
import Data.Symbol (class IsSymbol, SProxy)
import Effect.Aff (Aff)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Aff (awaitBody)
import Halogen.HTML as HH
import Halogen.VDom.Driver as VDom
import Web.HTML (HTMLElement)
import Type.Row as Row

type InputFields query input output
  = ( input :: input
    , child :: H.Component HH.HTML query input output Aff
    , targetElement :: Maybe HTMLElement
    )

type Input query input output
  = { | InputFields query input output }

type State query input output
  = { io :: Maybe (H.HalogenIO query output Aff)
    | InputFields query input output
    }

-- | An alternative to `slot` which mounts the child component to a specific
-- | HTMLElement in the DOM instead of within the parent component. Use this
-- | in place of `slot` -- it shares the same arguments, with an additional,
-- | optional `HTMLElement`. Your component will be mounted to the target element
-- | if provided, or the `<body>` tag otherwise.
-- |
-- | ```purs
-- | -- if `Nothing` is provided as the target HTMLElement, then the `<body>`
-- | -- tag will be used
-- | HH.div_ 
-- |   [ portal _modal unit Modal.component modalInput (Just element) handler ]
-- |
-- | -- for comparison, this is how you would mount the component _not_ via 
-- | -- a portal
-- | HH.div_
-- |   [ HH.slot _modal unit Modal.component modalInput handler ]
-- | ```
portal ::
  forall query action input output slots label slot _1.
  Row.Cons label (H.Slot query output slot) _1 slots =>
  IsSymbol label =>
  Ord slot =>
  SProxy label ->
  slot ->
  H.Component HH.HTML query input output Aff ->
  input ->
  Maybe HTMLElement ->
  (output -> Maybe action) ->
  H.ComponentHTML action slots Aff
portal label slot childComponent childInput htmlElement handler =
  handler
    # HH.slot label slot component
        { child: childComponent
        , input: childInput
        , targetElement: htmlElement
        }

component ::
  forall query input output m.
  MonadAff m =>
  H.Component HH.HTML query (Input query input output) output m
component =
  H.mkComponent
    { initialState
    , render
    , eval
    }
  where
  initialState :: Input query input output -> State query input output
  initialState { input, child, targetElement } =
    { input
    , child
    , targetElement
    , io: Nothing
    }

  eval ::
    H.HalogenQ query output (Input query input output)
      ~> H.HalogenM (State query input output) output () output m
  eval = case _ of
    H.Initialize a -> do
      state <- H.get
      -- Create a blocking mutable variable which will be updated with messages
      -- as they come in.
      var <- H.liftAff AVar.empty
      -- The target element can either be the one supplied by the user, or the
      -- document body. Either way, we'll run the sub-tree at the target and
      -- save the resulting interface.
      target <- maybe (H.liftAff awaitBody) pure state.targetElement
      io <- H.liftAff $ VDom.runUI state.child state.input target
      -- Subscribe to the child component's messages, writing them to the 
      -- variable. Multiple writes without a take will queue messages.
      H.liftAff $ io.subscribe
        $ consumer \msg -> do
            AVar.put msg var
            pure Nothing
      _ <-
        H.fork
          $ forever do
              msg <- H.liftAff (AVar.take var)
              eval (H.Action msg unit)
      H.modify_ _ { io = Just io }
      pure a
    H.Finalize a -> do
      state <- H.get
      for_ state.io (H.liftAff <<< _.dispose)
      pure a
    H.Receive input a -> pure a
    H.Action output a -> do
      H.raise output
      pure a
    H.Query query fail ->
      H.gets _.io
        >>= case _ of
            Nothing -> pure $ fail unit
            Just io -> H.liftAff $ unCoyoneda (\k q -> maybe' fail k <$> ioq io q) query

  -- We don't need to render anything; this component is explicitly meant to be
  -- passed through.
  render :: State query input output -> H.ComponentHTML output () m
  render _ = HH.text ""

  -- This is needed for a hint to the typechecker. Without it there's an
  -- idempotence issue with `a` when `HalogenIO` is taken from `State`.
  ioq :: forall a. H.HalogenIO query output Aff -> query a -> Aff (Maybe a)
  ioq = _.query
