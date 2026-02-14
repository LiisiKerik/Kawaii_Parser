{-|
Description: Assorted utilities.

* Assorted utilities.
-}
module Parser.Utilities (
  (<->),
  all_equal,
  between,
  check,
  construct_map,
  construct_set,
  lcm_all) where
  import Control.Monad.Except
  import Data.Foldable as Foldable
  import Data.Map as Map
  import Data.Set as Set
  -- | Concatenate strings with a whitespace in between.
  infixr 5 <->
  (<->) :: String -> String -> String
  s <-> t = s <> " " <> t
  -- | Checks if all elements of a list are equal. Returns @Nothing@ if all elements aren't equal, @Just Nothing@ if the list is
  -- empty and @Just (Just x)@ if all elements are equal to @x@.
  all_equal :: Eq t => [t] -> Maybe (Maybe t)
  all_equal x =
    case x of
      [] -> Just Nothing
      y : x' ->
        do
          check () (all ((==) y) x')
          Just (Just y)
  -- | Check if the value is in bounds.
  between :: Ord t => t -> t -> t -> Bool
  between lowest highest x = lowest <= x && highest >= x
  -- | Throw an error if the condition is not satisfied.
  check :: MonadError error f => error -> Bool -> f ()
  check err condition =
    case condition of
      False -> throwError err
      True -> return ()
  -- | Construct a map if all keys are different. Otherwise return @Nothing@.
  construct_map :: Ord t => [(t, u)] -> Maybe (Map t u)
  construct_map x =
    do
      let y = Map.fromList x
      check () (Foldable.length x == Map.size y)
      Just y
  -- | Construct a set if all elements are different. Otherwise return @Nothing@.
  construct_set :: Ord t => [t] -> Maybe (Set t)
  construct_set x =
    do
      let y = Set.fromList x
      check () (Foldable.length x == Set.size y)
      Just y
{-
  -- | Safe list indexation.
  element_at :: Int -> [t] -> Maybe t
  element_at j x =
    do
      check () (between 0 (Foldable.length x - 1) j)
      return (x !! j)
-}
  -- | Aggregate least common denominator.
  lcm_all :: (Foldable f, Integral t) => f t -> t
  lcm_all = Foldable.foldr lcm 1
{-
  -- | Swap Left and Right.
  swap_either :: Either t u -> Either u t
  swap_either x =
    case x of
      Left y -> Right y
      Right y -> Left y
-}