module Control.Monad.Logic.Class where

import Prelude (class Monad, class Monoid, Unit, const, map, mempty, pure, unit, (<<<), ($), (#), (<#>), (>>=))
import Control.Apply (lift2)
import Control.Monad.Except.Trans (ExceptT(..))
import Control.Monad.Maybe.Trans (MaybeT(..))
import Control.Monad.Reader.Trans (ReaderT(..))
import Control.Monad.State.Trans (StateT(..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer.Trans (WriterT(..))
import Control.Plus (class Plus, empty, (<|>))
import Data.Array ((:), uncons)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Machine.Mealy (MealyT)
import Data.Machine.Mealy (interleave, msplit) as Mealy
import Data.Tuple (Tuple(..))

class (Monad m, Plus m) <= MonadLogic m where
  msplit :: forall a. m a -> m (Maybe (Tuple a (m a)))
  interleave :: forall a. m a -> m a -> m a

fairConjunction :: forall m a b. MonadLogic m => m a -> (a -> m b) -> m b
fairConjunction m f =
  msplit m >>= maybe empty \(Tuple a m') -> f a `interleave` (m' >>- f)

infix 6 fairConjunction as >>-

ifte :: forall m a b. MonadLogic m => m a -> (a -> m b) -> m b -> m b
ifte t th el = msplit t >>= maybe el \(Tuple a m) -> th a <|> (m >>- th)

once :: forall m a. MonadLogic m => m a -> m a
once m = msplit m >>= maybe empty \(Tuple a _) -> pure a

when :: forall m a b. MonadLogic m => m a -> (a -> m b) -> m b
when m f = ifte m f empty

lnot :: forall m a. MonadLogic m => m a -> m Unit
lnot m = ifte (once m) (const empty) (pure unit)

instance monadLogicArray :: MonadLogic Array where
  msplit = pure <<< map (\{head, tail} -> Tuple head tail) <<< uncons
  interleave xs ys = uncons xs # maybe ys \{head, tail} ->
    head : interleave ys tail

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
