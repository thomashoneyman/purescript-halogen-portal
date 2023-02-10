-- | A container component which renders a sub-tree to a DOM node not in the
-- | tree. This is useful for when a child component needs to 'break out' of a
-- | parent, like dialogs, modals, and tooltips, especially if the parent has
-- | z-indexing or overflow: hidden set.
module Halogen.Portal where

import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Control.Monad.Rec.Class (forever)
import Data.Coyoneda (unCoyoneda)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), maybe, maybe')
import Data.Symbol (class IsSymbol)
import Effect.Aff (Aff, launchAff)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.Aff (awaitBody)
import Halogen.HTML as HH
import Halogen.Subscription as HS
import Halogen.VDom.Driver as VDom
import Prelude
import Type.Proxy (Proxy)
import Type.Row as Row
import Web.HTML (HTMLElement)

type InputFields query input output n
  = ( input :: input
    , child :: H.Component query input output n
    , targetElement :: Maybe HTMLElement
    )

type Input query input output n
  = { | InputFields query input output n }

type State query input output n
  = { io :: Maybe (H.HalogenIO query output Aff)
    | InputFields query input output n
    }

-- This wraps natural transformations into a type that is easier to use
-- with type inference.
--
-- A couple default handlers are provided below. If you have a custom monad,
-- you can make your own, or compose it with an existing handler:
--
-- ```purescript
-- newtype AppM a = AppM (ReaderT Env Aff a)
--
-- ntAppM :: AppM (NT AppM Aff)
-- ntAppM = AppM (ntReaderT <#> ntCompose (NT \(AppM ma) -> ma))
-- ```
--
-- Another option is to use `H.hoist` to lift your component into `ReaderT`.
newtype NT :: forall k. (k -> Type) -> (k -> Type) -> Type
newtype NT m n
  = NT (forall a. m a -> n a)

ntCompose :: forall h m n. NT h m -> NT m n -> NT h n
ntCompose (NT hm) (NT mn) = NT (hm >>> mn)

ntIdentity :: forall m. NT m m
ntIdentity = NT identity

ntAff :: forall m. MonadAff m => NT Aff m
ntAff = NT H.liftAff

ntReaderT :: forall r m n. Monad n => ReaderT r n (NT (ReaderT r m) m)
ntReaderT = asks \r -> NT \ma -> runReaderT ma r

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
-- |   [ portal ntIdentity _modal unit Modal.component modalInput (Just element) handler ]
-- |
-- | -- for comparison, this is how you would mount the component _not_ via
-- | -- a portal
-- | HH.div_
-- |   [ HH.slot _modal unit Modal.component modalInput handler ]
-- | ```
portal ::
  forall query action input output slots label slot _1 m n.
  Row.Cons label (H.Slot query output slot) _1 slots =>
  IsSymbol label =>
  Ord slot =>
  MonadAff m =>
  m (NT n Aff) ->
  Proxy label ->
  slot ->
  H.Component query input output n ->
  input ->
  Maybe HTMLElement ->
  (output -> action) ->
  H.ComponentHTML action slots m
portal contextualize label slot childComponent childInput htmlElement handler =
  handler
    # HH.slot label slot (component contextualize)
        { child: childComponent
        , input: childInput
        , targetElement: htmlElement
        }

-- | Run a portal component that is already in `Aff`.
portalAff ::
  forall m query action input output slots label slot _1.
  Row.Cons label (H.Slot query output slot) _1 slots =>
  IsSymbol label =>
  Ord slot =>
  MonadAff m =>
  Proxy label ->
  slot ->
  H.Component query input output Aff ->
  input ->
  Maybe HTMLElement ->
  (output -> action) ->
  H.ComponentHTML action slots m
portalAff = portal (pure ntIdentity)

-- | Run a portal component that is in `ReaderT r Aff` (for some context `r`).
portalReaderT ::
  forall m r query action input output slots label slot _1.
  Row.Cons label (H.Slot query output slot) _1 slots =>
  IsSymbol label =>
  Ord slot =>
  MonadAff m =>
  Proxy label ->
  slot ->
  H.Component query input output (ReaderT r Aff) ->
  input ->
  Maybe HTMLElement ->
  (output -> action) ->
  H.ComponentHTML action slots (ReaderT r m)
portalReaderT = portal ntReaderT

component ::
  forall query input output m n.
  MonadAff m =>
  m (NT n Aff) ->
  H.Component query (Input query input output n) output m
component contextualize =
  H.mkComponent
    { initialState
    , render
    , eval
    }
  where
  initialState :: Input query input output n -> State query input output n
  initialState { input, child, targetElement } =
    { input
    , child
    , targetElement
    , io: Nothing
    }

  eval ::
    H.HalogenQ query output (Input query input output n)
      ~> H.HalogenM (State query input output n) output () output m
  eval = case _ of
    H.Initialize a -> do
      NT context <- H.lift contextualize
      state <- H.get
      -- Create a blocking mutable variable which will be updated with messages
      -- as they come in.
      var <- H.liftAff AVar.empty
      -- The target element can either be the one supplied by the user, or the
      -- document body. Either way, we'll run the sub-tree at the target and
      -- save the resulting interface.
      target <- maybe (H.liftAff awaitBody) pure state.targetElement
      io <- H.liftAff $ VDom.runUI (H.hoist context state.child) state.input target
      -- Subscribe to the child component's messages, writing them to the
      -- variable. Multiple writes without a take will queue messages.
      _ <- liftEffect $ HS.subscribe io.messages \msg -> launchAff $ AVar.put msg var
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
    H.Receive _ a -> pure a
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
  render :: State query input output n -> H.ComponentHTML output () m
  render _ = HH.text ""

  -- This is needed for a hint to the typechecker. Without it there's an
  -- impredicativity issue with `a` when `HalogenIO` is taken from `State`.
  ioq :: forall a. H.HalogenIO query output Aff -> query a -> Aff (Maybe a)
  ioq = _.query
