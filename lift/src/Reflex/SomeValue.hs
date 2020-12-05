module Reflex.SomeValue where

import qualified Data.Dependent.Map                     as DMap

import Reflex

import Dom.CTag
import Dom.Pipe.EPipe
import Dom.SomeValue
import Dom.Value
import Dom.VTag

import Ground.Table ()


--------------------------------------------------------------------------------
-- * EPipe
--
newtype Wrap f a c =
  Wrap { unWrap :: f (a c) }

splitSVByCTag ::
  forall t c. (Reflex t)
  => CTag c
  -> Event t (PFallible SomeValue)
  -> EventSelectorG t CTag (Wrap PFallible SomeValueKinded)
splitSVByCTag et =
  fanG . fmap (\case
                  Right (SomeValue t v) -> DMap.singleton t (Wrap $ Right v)
                  Left e -> DMap.singleton et (Wrap $ Left e))

splitSVKByVTag ::
     forall t (c :: Con) v. (ReifyCTag c, Reflex t)
  => VTag v
  -> Event t (Wrap PFallible SomeValueKinded c)
  -> EventSelectorG t (VTag' ()) (Wrap PFallible (Value c))
splitSVKByVTag et =
  fanG . fmap (\case
                  Wrap (Right (SomeValueKinded t v)) ->
                    DMap.singleton  t (Wrap $ Right v)
                  Wrap (Left e) ->
                    DMap.singleton et (Wrap $ Left e))

