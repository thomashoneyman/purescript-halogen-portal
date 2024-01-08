-- | A container component which renders a sub-tree to a DOM node not in the
-- | tree. This is useful for when a child component needs to 'break out' of a
-- | parent, like dialogs, modals, and tooltips, especially if the parent has
-- | z-indexing or overflow: hidden set.
module Halogen.Portal where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Reader (ReaderT, asks, lift, runReaderT)
import Data.Coyoneda (hoistCoyoneda, unCoyoneda)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), maybe, maybe')
import Data.Symbol (class IsSymbol)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Aff (awaitBody)
import Halogen.HTML as HH
import Halogen.Store.Monad (StoreT(..))
import Halogen.VDom.Driver as VDom
import Type.Prelude (Proxy(..))
import Type.Proxy (Proxy)
import Type.Row as Row
import Web.HTML (HTMLElement)

type InputFields query input output n =
  ( input :: input
  , child :: H.Component query input output n
  , targetElement :: Maybe HTMLElement
  )

type Input query input output n = { | InputFields query input output n }

type State query input output n =
  { io :: Maybe (H.HalogenIO (Query input query) output Aff)
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
newtype NT :: (Type -> Type) -> (Type -> Type) -> Type
newtype NT m n = NT (forall a. m a -> n a)

ntCompose :: forall h m n. NT h m -> NT m n -> NT h n
ntCompose (NT hm) (NT mn) = NT (hm >>> mn)

ntIdentity :: forall m. NT m m
ntIdentity = NT identity

ntAff :: forall m. MonadAff m => NT Aff m
ntAff = NT H.liftAff

ntReaderT :: forall r m n. Monad n => ReaderT r n (NT (ReaderT r m) m)
ntReaderT = asks \r -> NT \ma -> runReaderT ma r

class (Monad m) <= PortalM m where
  toPortalAff :: m (NT m Aff)

instance PortalM Aff where
  toPortalAff = pure ntIdentity

instance (PortalM m) => PortalM (ReaderT r m) where
  toPortalAff :: ReaderT r m (NT (ReaderT r m) Aff)
  toPortalAff = lift2 ntCompose unReader toAff
    where
    unReader :: ReaderT r m (NT (ReaderT r m) m)
    unReader =
      asks \r -> do
        NT \ma -> do
          runReaderT ma r

    toAff = lift toPortalAff

instance (PortalM m) => PortalM (StoreT act r m) where
  toPortalAff :: _ (NT (StoreT act r m) Aff)
  toPortalAff = lift2 ntCompose unStore toAff
    where
    unStore :: _ (NT (StoreT act r m) m)
    unStore = StoreT $
      asks \r -> do
        NT \(StoreT ma) -> do
          runReaderT ma r

    toAff = lift toPortalAff

-- | `portal` but using the `PortalM` typeclass to allow for custom monads without a contextualize function.
portalM
  :: forall query action input output slots label slot _1 m
   . Row.Cons label (H.Slot query output slot) _1 slots
  => IsSymbol label
  => Ord slot
  => MonadAff m
  => PortalM m
  => Proxy label
  -> slot
  -> H.Component query input output m
  -> input
  -> Maybe HTMLElement
  -> (output -> action)
  -> H.ComponentHTML action slots m
portalM = portal toPortalAff

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
portal
  :: forall query action input output slots label slot _1 m n
   . Row.Cons label (H.Slot query output slot) _1 slots
  => IsSymbol label
  => Ord slot
  => MonadAff m
  => MonadAff n
  => m (NT n Aff)
  -> Proxy label
  -> slot
  -> H.Component query input output n
  -> input
  -> Maybe HTMLElement
  -> (output -> action)
  -> H.ComponentHTML action slots m
portal contextualize label slot childComponent childInput htmlElement handler =
  handler
    # HH.slot label slot (component contextualize)
        { child: childComponent
        , input: childInput
        , targetElement: htmlElement
        }

-- | Run a portal component that is already in `Aff`.
portalAff
  :: forall m query action input output slots label slot _1
   . Row.Cons label (H.Slot query output slot) _1 slots
  => IsSymbol label
  => Ord slot
  => MonadAff m
  => Proxy label
  -> slot
  -> H.Component query input output Aff
  -> input
  -> Maybe HTMLElement
  -> (output -> action)
  -> H.ComponentHTML action slots m
portalAff = portal (pure ntIdentity)

-- | Run a portal component that is in `ReaderT r Aff` (for some context `r`).
portalReaderT
  :: forall m r query action input output slots label slot _1
   . Row.Cons label (H.Slot query output slot) _1 slots
  => IsSymbol label
  => Ord slot
  => MonadAff m
  => Proxy label
  -> slot
  -> H.Component query input output (ReaderT r Aff)
  -> input
  -> Maybe HTMLElement
  -> (output -> action)
  -> H.ComponentHTML action slots (ReaderT r m)
portalReaderT = portal ntReaderT

data Query input query a = SetInput input a | ChildQuery (query a)

_content :: Proxy "content"
_content = Proxy @"content"

-- wraps the portalled component and provides a SetInput query
-- that can be used by the Portal component to update the child's
-- input when it receives new values from the parent
wrapper
  :: forall query input output m
   . MonadAff m
  => H.Component (Query input query) (State query input output m) output m
wrapper = H.mkComponent
  { initialState: identity
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleQuery = handleQuery
      , handleAction = H.raise
      }
  }

  where

  render { input, child } = HH.slot _content unit child input identity

  handleQuery :: forall action a. Query input query a -> H.HalogenM _ action _ output m (Maybe a)
  handleQuery = case _ of
    SetInput input a -> do
      H.modify_ _ { input = input }
      pure $ Just a
    ChildQuery query -> do
      res <- H.query _content unit query
      pure res

component
  :: forall query input output m n
   . MonadAff m
  => MonadAff n
  => m (NT n Aff)
  -> H.Component query (Input query input output n) output m
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

  eval
    :: H.HalogenQ query output (Input query input output n)
         ~> H.HalogenM (State query input output n) output () output m
  eval = case _ of
    H.Initialize a -> do
      NT context <- H.lift contextualize
      state <- H.get
      -- The target element can either be the one supplied by the user, or the
      -- document body. Either way, we'll run the sub-tree at the target and
      -- save the resulting interface.
      target <- maybe (H.liftAff awaitBody) pure state.targetElement
      io <- H.liftAff $ VDom.runUI (H.hoist context wrapper) state target
      -- Subscribe to the child component's messages
      _ <- H.subscribe io.messages
      H.modify_ _ { io = Just io }
      pure a
    H.Finalize a -> do
      state <- H.get
      for_ state.io (H.liftAff <<< _.dispose)
      pure a
    H.Receive { input } a -> H.gets _.io
      >>= case _ of
        Nothing -> pure a
        Just io -> do
          void $ H.liftAff $ (ioq io) (SetInput input a)
          pure a
    H.Action output a -> do
      H.raise output
      pure a
    H.Query query fail ->
      H.gets _.io
        >>= case _ of
          Nothing -> pure $ fail unit
          Just io -> H.liftAff $ unCoyoneda (\k q -> maybe' fail k <$> ioq io q) (hoistCoyoneda ChildQuery query)

  -- We don't need to render anything; this component is explicitly meant to be
  -- passed through.
  render :: State query input output n -> H.ComponentHTML output () m
  render _ = HH.text ""

  -- This is needed for a hint to the typechecker. Without it there's an
  -- impredicativity issue with `a` when `HalogenIO` is taken from `State`.
  ioq :: forall a. H.HalogenIO (Query input query) output Aff -> (Query input query) a -> Aff (Maybe a)
  ioq = _.query
