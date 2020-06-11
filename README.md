[lp]: https://en.wikipedia.org/wiki/Logic_programming
[LogicT.pdf]: http://okmij.org/ftp/papers/LogicT.pdf
[logict]: https://hackage.haskell.org/package/logict

# [Logic programming][lp] in PureScript
Based on the paper [Backtracking, Interleaving, and TerminatingMonad Transformers][LogicT.pdf] and the similair Haskell package [logict].

---

- `empty` is a failed computation
- `pure` is a successful computation
- `<|>` and `interleave` is disjunction of goals
- `>>=` and `>>-` is conjunction of goals

## Basic usage
The `MonadLogic` type class represents non-deterministic computations.

Logic program with four successful solutions; `10`, `20`, `30` and `41`:
```purescript
logicNumber :: forall m. MonadLogic m => m Int
logicNumber = pure 10 <|> pure 20 <|> pure 30 <|> pure 41
```

Logic program with four successful solutions; `11`, `21`, `31` and `42`:
```purescript
logicNumberSucc :: forall m. MonadLogic m => m Int
logicNumberSucc = logicNumber >>= \i -> pure (i + 1)
```

Logic program resulting in only even integers as valid solutions:
```purescript
logicEven :: forall m. MonadLogic m => Int -> m Int
logicEven i = do
  guard (i `mod` 2 == 0)
  pure i
```

A logic program that filters out even integers, multiplies them with `100`. If there are no even integers, the computation succeeds with `1337`:
```purescript
logicSoftCut :: forall m. MonadLogic m => m Int -> m Int
logicSoftCut xs = do
  ifte (xs >>= logicEven)
    (\i -> pure (i * 100))
    (pure 1337)
```
`ifte` represents the logic programming paradigm "negation as finite failure", which performs a logical computation when some other computation fails.

## Fair disjunction
Consider the `everyOtherInt` computation, it contains infinite solutions:
```purescript
everyOtherInt :: forall m. MonadLogic m => Int -> m Int
everyOtherInt = pure >=> \i -> pure i <|> everyOtherInt (i + 2)
```
*Note: This could in theory be simplified but PureScript is strict so `pure >=>` allows lazy data structures to defer evaluation lest we run out of memory.*

```purescript
oddIntegers :: forall m. MonadLogic m => m Int
oddIntegers = everyOtherInt 1

evenIntegers :: forall m. MonadLogic m => m Int
evenIntegers = (oddIntegers <|> pure 2) >>= logicEven
```
`evenIntegers` should in theory succeed with `2` but since `oddIntegers` is infinite the computation diverges.

`interleave` is a "fair" version of `<|>` that interleaves solutions in two computations. This is important when one computation is potentially infinite.

```purescript
evenIntegers :: forall m. MonadLogic m => m Int
evenIntegers = (oddIntegers `interleave` pure 2) >>= logicEven
```
`evenIntegers` now succeeds with `2`.

## Fair conjunction
Consider the `integers` computation, it contains infinite solutions:
```purescript
integers :: forall m. MonadLogic m => m Int
integers = (pure 1 <|> pure 2) >>= everyOtherInt
```
The following computation diverges because all even integers are followed by all odd integers:
```purescript
evenIntegers :: forall m. MonadLogic m => m Int
evenIntegers = integers >>= logicEven
```

`>>-` is a "fair" version of `>>=` that interleaves resulting solutions.
```purescript
evenIntegers :: forall m. MonadLogic m => m Int
evenIntegers = integers >>- logicEven
```
`evenIntegers` now succeeds with even integers.

## Full example: Primes
A program that prints the first ten primes to the console:
```purescript
module Ex.Primes where

import Prelude
import Control.Monad.List.Trans as ListT
import Control.Monad.Logic.Class (class MonadLogic)
import Control.Monad.Logic.Class as Logic
import Control.MonadZero (empty, guard, (<|>))
import Effect (Effect)
import Effect.Class.Console (logShow)

integers :: forall m. MonadLogic m => Int -> m Int
integers = pure >=> \i -> pure i <|> integers (i + 1)

range :: forall m. MonadLogic m => Int -> Int -> m Int
range start stop =
  if start > stop then
    empty
  else
    pure start >>= \i -> pure i <|> range (i + 1) stop

divisors :: forall m. MonadLogic m => Int -> m Int
divisors n = do
  d <- range 2 (n - 1)
  guard $ n `mod` d == 0
  pure d

primes :: forall m. MonadLogic m => m Int
primes = do
  n <- integers 2
  Logic.ifte (Logic.once $ divisors n)
    (const empty)
    (pure n)

main :: Effect Unit
main = ListT.runListTRec $ ListT.take 10 $ logShow =<< primes
```
`once` is a control structure to prune results. We don't care about the divisors themselves, only that if there is at least one then `n` is not a prime. Calculating the rest of the divisors is wasteful so if we can stop the computation that's useful as a performance optimization.
