module Codec.Json.Unidirectional.Value.SortRowList where

import Prim.Int as PInt
import Prim.Ordering as POrdering
import Prim.RowList as RL

-- | The combination of `SortRows`, `InsertionSort`, and `InsertRow` implement this algorithm
-- | to sort the list or `FromProp` by insertion order and then by the label's alphabetical ordering.
-- | Since `RowToList` returns a list of rows in alphabetical ordering, we sort by the label's alphabetical
-- | ordering by maintaining the original position
-- | ```
-- | isort :: forall a. (a -> Int) -> List a -> List a
-- | isort f = flip go Nil
-- |   where
-- |   go next acc = case next, acc of
-- |     Nil, _ -> acc
-- |     Cons h t, Nil -> go t $ Cons h acc
-- |     Cons h1 t1, Cons h2 t2
-- |       | f h < f h2 = go t2 $ Cons h1 acc
-- |       | otherwise = Cons h2 $ go next t2
-- | ````
-- | For proof, see
-- | https://try.purescript.org/?code=FAWw9gJgrgNgpgAgLIEMCWA7BB3AFnAJzmGDRAAcwCAXBABSJigmNIqtoBEVqUA6ADJoAztT4AVAJ7k4whAAoho%2BXz4BKNW0o0EAUQBm%2BuAGNa8g0dOay22hZNiAwjBTDhfR2AzCw8BTDAAc2t2HXECSTooIgBlYwI0clpXBHDI6Lg4hKSSEHQsAC4CvUMHBABVDDRqUHyEAF5UiKjY%2BMSxIgwWAgaAHl6m9NbssWxq3E9vX0QIMGAEBADAhAASBGFcMGxVhBEOAHIEeYWdybl5cQQABgQAFjUjk9Ovc8uARgQAJgfjhbWzhSXT5fH5Pf4vQEIADMCAAHKCTmsAHJoGAkAC06NSCHwRF2cigwjgEAQ1DApIIKGMAGtgJjSfgEFQ0IFMCgYLsunAAB5M-QMxBweAgOAYMz6NAEUQIACSYs09JQXQFCCFcBFYt21CJMH58iJxi8JLl1E0EB4KGxjUuJtlYtI3l4GGMiF0AEdsXhCIKPY1jK5EAB9AA0CEDfOx4eEocuUeB6IAfOsGo1hJ8HaIlS6EAB5Agky5evGGigoPF%2BgNh0PhsD82PrGNh9bxpMl8hlxByNMZp3ZmKbbaF3GdgeQtC7b4NBAAIguM4QvSTGy2uwXSen88X61HaGBW%2BnamnJD2NEOjRPZgAOnPwwA3dlQOAPRMIe9MJ-Hnw6IoIfRUdkcvwCiWi%2BJrPkmSjJAgL6QQgKCkF%2BtD8o0%2BgwIkCCBOSKJogsRbEAsmEIBgPLJMYxhTv6RJESRoZUuRta-Ag2HVtBSZ0YxAK4KSobYaxGHkrQ4LeDicFkRxEK4B81BvKGnHAtQ6ZPAsAA%2Bv44h8Az8rgLb8aSHxCXIkmicYjEqUy1C4mMVEvnJOyEcR3K0ApJBAA
class IntThenAlphaSortedRowList :: RL.RowList Type -> RL.RowList Type -> Constraint
class IntThenAlphaSortedRowList alphaSortedRows intThenAlphaSortedRows | alphaSortedRows -> intThenAlphaSortedRows

class ToOrdering :: forall k. k -> Int -> Constraint
class ToOrdering value order | value -> order

-- `flip go Nil`
instance InsertionSort unsorted RL.Nil sorted => IntThenAlphaSortedRowList unsorted sorted

class InsertionSort :: RL.RowList Type -> RL.RowList Type -> RL.RowList Type -> Constraint
class InsertionSort unsorted acc sorted | unsorted acc -> sorted

-- case next, acc of 
--   Nil, _ -> acc
instance InsertionSort RL.Nil acc acc

-- case next, acc of
--   Cons h t, Nil -> go t $ Cons h acc
instance
  ( InsertionSort tail (RL.Cons sym a RL.Nil) sorted
  ) =>
  InsertionSort (RL.Cons sym a tail) RL.Nil sorted

-- case next, acc of
--   Cons h1 t1, Cons h2 t2
--     | h1 < h2 -> go t1 $ Cons h1 acc
--     | otherwise -> Cons h2 $ go next t2
instance
  ( ToOrdering a1 insertionOrder1
  , ToOrdering a2 insertionOrder2
  , PInt.Compare insertionOrder1 insertionOrder1 res
  , InsertRow res sym1 a1 tail1 sym2 a2 tail2 sorted
  ) =>
  InsertionSort (RL.Cons sym1 a1 tail1) (RL.Cons sym2 a2 tail2) sorted

class InsertRow :: POrdering.Ordering -> Symbol -> Type -> RL.RowList Type -> Symbol -> Type -> RL.RowList Type -> RL.RowList Type -> Constraint
class
  InsertRow ord nextSym nextA nextTail accSym accA accTail newAcc
  | ord nextSym nextA nextTail accSym accA accTail -> newAcc

-- case next, acc of
--   Cons h1 t1, Cons h2 t2
--     | h1 < h2 -> go t1 $ Cons h1 acc
--     | ...
instance
  ( InsertionSort nextTail (RL.Cons nextSym nextA (RL.Cons accSym accA accTail)) sorted
  ) =>
  InsertRow POrdering.LT nextSym nextA nextTail {- , -} accSym accA accTail {- = -} sorted

-- case next, acc of
--   Cons h1 t1, Cons h2 t2
--     | .. 
--     | otherwise -> Cons h2 $ go next t2
instance
  ( InsertionSort (RL.Cons nextSym nextA nextTail) accTail sorted
  ) =>
  InsertRow POrdering.EQ nextSym nextA nextTail {- , -} accSym accA accTail {- = -} (RL.Cons accSym accA sorted)

-- case next, acc of
--   Cons h1 t1, Cons h2 t2
--     | .. 
--     | otherwise -> Cons h2 $ go next t2
instance
  ( InsertionSort (RL.Cons nextSym nextA nextTail) {- , -}  accTail {- = -}  sorted
  ) =>
  InsertRow POrdering.GT nextSym nextA nextTail {- , -} accSym accA accTail {- = -} (RL.Cons accSym accA sorted)
