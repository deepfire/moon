module Reflex.SomeValue where

import qualified Data.Dependent.Map                     as DMap

import Reflex

import Dom.CTag
import Dom.SomeValue
import Dom.Value
import Dom.VTag

import Ground.Table ()


splitSVByKinds ::
  forall t. (Reflex t)
  => Event t SomeValue
  -> EventSelectorG t CTag SomeValueKinded
splitSVByKinds =
  fanG . fmap (\(SomeValue t v) -> DMap.singleton t v)

splitSVKByTypes ::
     forall (c :: Con) t. (ReifyCTag c, Reflex t)
  => Event t (SomeValueKinded c)
  -> EventSelectorG t (VTag' ()) (Value c)
splitSVKByTypes =
  fanG . fmap (\(SomeValueKinded t v) -> DMap.singleton t v)

