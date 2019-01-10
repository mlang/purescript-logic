module Test.Main where

import Prelude
import Benchotron.Core (Benchmark, benchFn, mkBenchmark)
import Benchotron.UI.Console (runSuite)
import Control.Monad.Logic.Class
import Data.Array (reverse, (..))
import Effect (Effect)
import Test.QuickCheck (quickCheck, (==?))
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (Gen, vectorOf)

benchInterleaveArray :: Benchmark
benchInterleaveArray = mkBenchmark
  { slug: "interleaveArray"
  , title: "Interleaving two arrays"
  , sizes: (1..5) <#> (_ * 1000)
  , sizeInterpretation: "Number of elements in the array"
  , inputsPerSize: 2
  , gen: \n -> vectorOf n (arbitrary :: Gen Unit)
  , functions: [ benchFn "interleave" interleave ]
  }

main :: Effect Unit
main = do
  quickCheck \(xs :: Array Unit) -> xs `interleave` [] ==? xs
  quickCheck \(xs :: Array Unit) -> [] `interleave` xs ==? xs
  quickCheck \(xs :: Array Int) ->
    let ys = (_ / 2) <$> xs in
    (reverse xs) `interleave` (reverse ys) ==? reverse (ys `interleave` xs)
  quickCheck \(a :: Int) (b :: Int) -> [a] `interleave` [b] ==? [a,b]
  runSuite [benchInterleaveArray]

