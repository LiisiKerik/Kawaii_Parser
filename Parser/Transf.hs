{-# OPTIONS_GHC -Wall #-}
{-|
Description: Type synonyms and run functions for pairwise compositions of reader, writer and state transformers.

* RST
* RWT
* WST
-}
module Parser.Transf (RST, RWT, WST, evalRST, evalWST, execRST, execRWT, execWST, runRST, runRWT, runWST) where
  import Control.Monad.RWS.Strict (RWST (..))
  -- | The composition of reader and state transformers.
  type RST env = RWST env ()
  -- | The composition of reader and writer transformers.
  type RWT env res = RWST env res ()
  -- | The composition of writer and state transformers.
  type WST = RWST ()
  -- | Discards the end state.
  evalRST :: Functor f => RST env state f t -> env -> state -> f t
  evalRST (RWST f) env st = (\(x, _, _) -> x) <$> f env st
  -- | Discards the end state.
  evalWST :: Functor f => WST res state f t -> state -> f (t, res)
  evalWST (RWST f) st = (\(x, _, res) -> (x, res)) <$> f () st
  -- | Discards the output.
  execRST :: Functor f => RST env state f t -> env -> state -> f state
  execRST (RWST f) env st = (\(_, st', _) -> st') <$> f env st
  -- | Discards the output.
  execRWT :: Functor f => RWT env res f t -> env -> f res
  execRWT (RWST f) env = (\(_, (), res) -> res) <$> f env ()
  -- | Discards the output.
  execWST :: Functor f => WST res state f t -> state -> f (state, res)
  execWST (RWST f) st = (\(_, st', res) -> (st', res)) <$> f () st
  -- | Runs the RS transformer.
  runRST :: Functor f => RST env state f t -> env -> state -> f (t, state)
  runRST (RWST f) env st = (\(x, st', ()) -> (x, st')) <$> f env st
  -- | Runs the RW transformer.
  runRWT :: Functor f => RWT env res f t -> env -> f (t, res)
  runRWT (RWST f) env = (\(x, (), res) -> (x, res)) <$> f env ()
  -- | Runs the WS transformer.
  runWST :: WST res state f t -> state -> f (t, state, res)
  runWST (RWST f) = f ()