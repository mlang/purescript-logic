module Control.Monad.Logic.Class (
  class MonadLogic, msplit, interleave
, fairConjunction, (>>-), ifte, once, when, lnot
, reflect
) where

import Prelude (class Monad, class Monoid, Unit, const, map, mempty, pure, unit, (<<<), ($), (#), (<#>), (>>=))
import Control.Apply (lift2)
import Control.Monad.Except.Trans (ExceptT(..))
import Control.Monad.Maybe.Trans (MaybeT(..))
import Control.Monad.Reader.Trans (ReaderT(..))
import Control.Monad.State.Trans (StateT(..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer.Trans (WriterT(..))
import Control.MonadPlus (class MonadPlus, empty, (<|>))
import Data.Array (uncons) as Array
import Data.CatList (CatList)
import Data.CatList (cons, singleton, uncons) as CatList
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Machine.Mealy (MealyT)
import Data.Machine.Mealy (interleave, msplit) as Mealy
import Data.Tuple (Tuple(..))

-- | The `MonadLogic` type class extends `MonadPlus` with two members
-- | `msplit` and `interleave`.
-- |
-- | Instances must satisfy the following laws in addition to the
-- | `MonadPlus` laws:
-- |
-- | - `msplit empty ≃ pure Nothing`
-- | - `msplit (alt (pure a) m) ≃ pure (Just (Tuple a m))`
-- |
-- | Justification for `interleave` paraphrasing the paper
-- | [LogicT.pdf](http://okmij.org/ftp/papers/LogicT.pdf):
-- |
-- | `alt` satisfies the law:
-- |
-- | - `alt x y ≃ x`
-- |
-- | whenever `x` is a computation that can backtrack arbitrarily many
-- | times. This law is undesirable as it compromises completeness. 
-- | It would be useful to have a new primitive `interleave` such that:
-- |
-- | ```purescript
-- | take 1 do
-- |   x <- (iterate (_ + 2) 1) `interleave` (pure 10)
-- |   if even x then pure x else empty
-- | ```
-- |
-- | succeeds with the answer `10`.
class MonadPlus m <= MonadLogic m where
  msplit :: forall a. m a -> m (Maybe (Tuple a (m a)))
  interleave :: forall a. m a -> m a -> m a

-- | `fairConjunction` interleaves the results such that
-- | 
-- | ```purescript
-- | (a <|> b) >>- k ≃ (a >>- k) <|> (b >>- k)
-- | ```
-- | 
-- | even when `a >>- k` is a computation that can backtrack arbitrarily many times.
fairConjunction :: forall m a b. MonadLogic m => m a -> (a -> m b) -> m b
fairConjunction m f =
  msplit m >>= maybe empty \(Tuple a m') -> f a `interleave` (m' >>- f)

infix 6 fairConjunction as >>-

-- | `ifte` is a soft cut (conditional) control structure like if-then-else but for logic computations.
-- |
-- | ex:
-- | ```purescript
-- | ifte (pure 10 <|> pure 20) (\i -> pure (i + 1)) (pure 30) = pure 11 <|> pure 21
-- | ```
-- |
-- | ```purescript
-- | ifte empty (\i -> pure (i + 1)) (pure 30) = pure 30
-- | ```
ifte :: forall m a b. MonadLogic m => m a -> (a -> m b) -> m b -> m b
ifte t th el = msplit t >>= maybe el \(Tuple a m) -> th a <|> (m >>- th)

-- | `once` is a pruning control structure returning the first result.
-- |
-- | ex:
-- | ```purescript
-- | once (pure 10 <|> pure 20 <|> pure 30) = pure 10
-- | ```
once :: forall m a. MonadLogic m => m a -> m a
once m = msplit m >>= maybe empty \(Tuple a _) -> pure a

when :: forall m a b. MonadLogic m => m a -> (a -> m b) -> m b
when m f = ifte m f empty

-- | `lnot` is a negation as failure control structure short circuiting a computation when another computation succeeds.
-- |
-- | ex:
-- | ```purescript
-- | do
-- |   lnot empty
-- |   pure 10
-- | = pure 10
-- | ```
-- |
-- | ```purescript
-- | do
-- |   lnot (pure 1)
-- |   pure 10
-- | = empty
-- | ```
lnot :: forall m a. MonadLogic m => m a -> m Unit
lnot m = ifte (once m) (const empty) (pure unit)

reflect :: forall m a. MonadLogic m => Maybe (Tuple a (m a)) -> m a
reflect Nothing = empty
reflect (Just (Tuple a m)) = pure a <|> m
instance monadLogicArray :: MonadLogic Array where
  msplit xs = [Array.uncons xs <#> \{head, tail} -> Tuple head tail]
  interleave = interleaveArray

instance monadLogicCatList :: MonadLogic CatList where
  msplit = CatList.singleton <<< CatList.uncons
  interleave xs ys = CatList.uncons xs # maybe ys \(Tuple a xs') ->
    CatList.cons a $ ys `interleave` xs'

instance monadLogicExceptT :: (Monoid e, MonadLogic m) => MonadLogic (ExceptT e m) where
  msplit (ExceptT m) = ExceptT $ msplit m <#> maybe
    (Right Nothing) \(Tuple a m') -> map (\x -> Just (Tuple x (ExceptT m'))) a
  interleave (ExceptT m1) (ExceptT m2) = ExceptT $ m1 `interleave` m2

instance monadLogicMaybeT :: MonadLogic m => MonadLogic (MaybeT m) where
  msplit (MaybeT m) = MaybeT $ msplit m <#> maybe
    (Just Nothing) \(Tuple a m') -> map (\x -> Just (Tuple x (MaybeT m'))) a
  interleave (MaybeT m1) (MaybeT m2) = MaybeT $ m1 `interleave` m2

instance monadLogicMealyT :: Monad f => MonadLogic (MealyT f s) where
  msplit = Mealy.msplit
  interleave = Mealy.interleave

instance monadLogicReaderT :: MonadLogic m => MonadLogic (ReaderT e m) where
  msplit (ReaderT m) = ReaderT $ (map <<< map <<< map) lift <<< msplit <<< m
  interleave (ReaderT m1) (ReaderT m2) = ReaderT $ lift2 interleave m1 m2

instance monadLogicStateT :: MonadLogic m => MonadLogic (StateT s m) where
  msplit (StateT m) = StateT \s -> msplit (m s) <#> maybe
    (Tuple Nothing s) \(Tuple (Tuple a s') m') ->
      Tuple (Just (Tuple a (StateT $ const m'))) s'
  interleave (StateT m1) (StateT m2) = StateT $ lift2 interleave m1 m2

instance monadLogicWriterT :: (Monoid w, MonadLogic m) => MonadLogic (WriterT w m) where
  msplit (WriterT m) = WriterT $ msplit m <#> maybe
    (Tuple Nothing mempty) \(Tuple (Tuple a w) m') ->
      Tuple (Just (Tuple a (WriterT m'))) w
  interleave (WriterT m1) (WriterT m2) = WriterT $ m1 `interleave` m2

foreign import interleaveArray :: forall a. Array a -> Array a -> Array a
