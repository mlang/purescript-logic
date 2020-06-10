module Control.Monad.Logic.Class (
  class MonadLogic, msplit, interleave
, fairConjunction, (>>-), ifte, once, when, lnot
, reflect
) where

import Prelude (class Monad, class Monoid, Unit, const, map, mempty, pure, unit, (<<<), ($), (#), (<#>), (>>=), bind)
import Control.Apply (lift2)
import Control.Monad.Except.Trans (ExceptT(..))
import Control.Monad.List.Trans (ListT)
import Control.Monad.List.Trans as ListT
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

class MonadPlus m <= MonadLogic m where
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

instance monadLogicListT :: Monad f => MonadLogic (ListT f) where
  msplit = lift <<< ListT.uncons
  interleave xs ys = do
    x <- msplit xs
    y <- msplit ys
    case x, y of
      Nothing, _ -> ys
      _, Nothing -> xs
      Just (Tuple xh xt), Just (Tuple yh yt) -> pure xh <|> pure yh <|> interleave xt yt

foreign import interleaveArray :: forall a. Array a -> Array a -> Array a
