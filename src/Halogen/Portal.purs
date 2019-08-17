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
import Effect.Aff (Aff, error, forkAff, killFiber)
import Effect.Aff.Bus (BusRW)
import Effect.Aff.Bus as Bus
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Aff (awaitBody)
import Halogen.HTML as HH
import Halogen.Query.EventSource as ES
import Halogen.VDom.Driver as VDom
import Web.HTML (HTMLElement)

type InputRep query input output
  = ( input :: input
    , child :: H.Component HH.HTML query input output Aff
    , targetElement :: Maybe HTMLElement
    )

type Input query input output
  = { | InputRep query input output }

type State query input output
  = { io :: Maybe (H.HalogenIO query output Aff)
    , bus :: Maybe (BusRW output)
    | InputRep query input output
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
    , bus: Nothing
    }

  eval
    :: H.HalogenQ query output (Input query input output)
    ~> H.HalogenM (State query input output) output () output m
  eval = case _ of
    H.Initialize a -> do
      state <- H.get
      -- The target element can either be the one supplied by the user, or the
      -- document body. Either way, we'll run the sub-tree at the target and
      -- save the resulting interface.
      target <- maybe (H.liftAff awaitBody) pure state.targetElement
      io <- H.liftAff $ VDom.runUI state.child state.input target
      H.modify_ _ { io = Just io }
      -- Subscribe to a new event bus, which will run each time a new output
      -- is emitted by the child component.
      _ <- H.subscribe <<< busEventSource =<< H.liftEffect Bus.make
      -- Subscribe to the child component, writing to the bus every time a
      -- message arises. This indirection through the bus is necessary because
      -- the component is being run via Aff, not HalogenM
      H.liftAff $ io.subscribe
        $ consumer \msg -> do
            for_ state.bus (Bus.write msg)
            pure Nothing
      pure a
    H.Finalize a -> do
      state <- H.get
      for_ state.io (H.liftAff <<< _.dispose)
      pure a
    H.Receive input a ->
      pure a
    H.Action msg a -> do
      H.raise msg
      pure a
    H.Query cq fail ->
      H.gets _.io >>= case _ of
        Nothing ->
          pure $ fail unit
        Just io ->
          H.liftAff $ unCoyoneda (\k q -> maybe' fail k <$> ioq io q) cq

  -- This is needed for a hint to the typechecker, without it there's an
  -- idempotence issue with `a` when `HalogenIO` is taken from the `State`
  -- record
  ioq :: ∀ a. H.HalogenIO query output Aff -> query a -> Aff (Maybe a)
  ioq = _.query

  -- We don't need to render anything; this component is explicitly meant to be
  -- passed through.
  render :: State query input output -> H.ComponentHTML output () m
  render _ = HH.text ""

-- Create an event source from a many-to-many bus, which a Halogen component
-- can subscribe to.
busEventSource :: forall m r act. MonadAff m => Bus.BusR' r act -> ES.EventSource m act
busEventSource bus =
  ES.affEventSource \emitter -> do
    fiber <- forkAff $ forever $ ES.emit emitter =<< Bus.read bus
    pure (ES.Finalizer (killFiber (error "Event source closed") fiber))
