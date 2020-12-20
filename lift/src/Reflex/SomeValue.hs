module Reflex.SomeValue where

import qualified Data.Dependent.Map                     as DMap

import Reflex

import Dom.CTag
import Dom.Cap
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

data CapValue c a =
  CapValue
  { cvCaps   :: Caps a
  , _cvValue :: Value c a
  }

cvValue :: CapValue c a -> Value c a
cvValue = _cvValue
{-# INLINE cvValue #-}

stripCapValue :: CapValue c a -> Repr c a
stripCapValue = stripValue . cvValue
{-# INLINE stripCapValue #-}

splitSVByCTag ::
  forall t c. (Reflex t)
  => CTag c
  -> Event t (PFallible SomeValue)
  -> EventSelectorG t CTag (Wrap PFallible SomeValueKinded)
splitSVByCTag et =
  fanG . fmap (\case
                  Right (SV t v) -> DMap.singleton t (Wrap $ Right v)
                  Left e -> DMap.singleton et (Wrap $ Left e))

splitSVKByVTag ::
     forall t (c :: Con) v. (ReifyCTag c, Reflex t)
  => VTag v
  -> Event t (Wrap PFallible SomeValueKinded c)
  -> EventSelectorG t (VTag' ()) (Wrap PFallible (CapValue c))
splitSVKByVTag et =
  fanG . fmap (\case
                  Wrap (Right (SVK t s v)) ->
                    DMap.singleton  t (Wrap $ Right (CapValue s v))
                  Wrap (Left e) ->
                    DMap.singleton et (Wrap $ Left e))

