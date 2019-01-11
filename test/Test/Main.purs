module Test.Main where

import Prelude (class Eq, Unit, discard, pure, (#), (*), (/), (<#>), (<$>), (==))
import Benchotron.Core (Benchmark, benchFn, mkBenchmark)
import Benchotron.UI.Console (runSuite)
import Control.MonadPlus (empty, (<|>))
import Control.Monad.Logic.Class
import Data.Array (reverse, uncons, (..), (:))
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Test.QuickCheck (quickCheck, quickCheck', (==?))
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, vectorOf)
import Test.QuickCheck.Laws (A)
import Type.Proxy (Proxy2(..))

checkMonadLogic
  :: forall m. MonadLogic m => Arbitrary (m A) => Eq (m (Maybe (Tuple A (m A))))
  => Proxy2 m -> Effect Unit
checkMonadLogic _ = do
  log "Checking laws for MonadLogic"
  quickCheck' 1000 msplitEmpty
  quickCheck' 1000 msplitPlus
 where
  msplitEmpty :: A -> m A -> Boolean
  msplitEmpty a m = msplit (empty :: m A) == pure Nothing
  msplitPlus :: A -> m A -> Boolean
  msplitPlus a m = msplit (pure a <|> m) == pure (Just (Tuple a m))

benchInterleaveArray :: Benchmark
benchInterleaveArray = mkBenchmark
  { slug: "interleaveArray"
  , title: "Interleaving two arrays"
  , sizes: (1..3) <#> (_ * 1000)
  , sizeInterpretation: "Number of elements in the array"
  , inputsPerSize: 1
  , gen: \n -> vectorOf n (arbitrary :: Gen Unit)
  , functions: [ benchFn "interleaveForeign" \xs -> xs `interleave` xs
               , benchFn "interleaveNative" \xs -> xs `interleaveArray` xs
               ]
  }

main :: Effect Unit
main = do
  checkMonadLogic arrayProxy
  quickCheck \(xs :: Array Unit) -> xs `interleave` [] ==? xs
  quickCheck \(xs :: Array Unit) -> [] `interleave` xs ==? xs
  log "Checking reversable interleave"
  quickCheck' 1000 \(xs :: Array Int) ->
    let ys = (_ / 2) <$> xs in
    (reverse xs) `interleave` (reverse ys) ==? reverse (ys `interleave` xs)
  log "Checking foreign intereleaveArray against native implementation"
  quickCheck' 1000 \(xs :: Array Int) ys ->
    xs `interleave` ys ==? xs `interleaveArray` ys
  runSuite [benchInterleaveArray]
 where
  arrayProxy = Proxy2 :: Proxy2 Array

interleaveArray :: forall a. Array a -> Array a -> Array a
interleaveArray xs ys = uncons xs # maybe ys \{head, tail} ->
  head : interleaveArray ys tail

