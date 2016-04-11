{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module    : Data.Kanji.Types.Internal
-- Copyright : (c) Colin Woodbury, 2015, 2016
-- License   : GPL3
-- Maintainer: Colin Woodbury <colingw@gmail.com>
--
-- Instances needed for using `each` over `Text` types.
-- This code is borrowed from `microlens-platform`:
-- http://hackage.haskell.org/package/microlens-platform

module Data.Kanji.Types.Internal () where

import           Data.Monoid (Endo)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Lens.Micro
import           Lens.Micro.Internal

---

instance (a ~ Char, b ~ Char) => Each T.Text T.Text a b where
  each = strictText
  {-# INLINE each #-}

instance (a ~ Char, b ~ Char) => Each TL.Text TL.Text a b where
  each = lazyText
  {-# INLINE each #-}

strictUnpacked :: Lens' T.Text String
strictUnpacked f t = T.pack <$> f (T.unpack t)
{-# INLINE strictUnpacked #-}

strictText :: Traversal' T.Text Char
strictText = strictUnpacked . traversed
{-# INLINE [0] strictText #-}

{-# RULES
"strict text -> map"    strictText = sets T.map        :: ASetter' T.Text Char;
"strict text -> foldr"  strictText = foldring T.foldr  :: Getting (Endo r) T.Text Char;
  #-}

lazyUnpacked :: Lens' TL.Text String
lazyUnpacked f t = TL.pack <$> f (TL.unpack t)
{-# INLINE lazyUnpacked #-}

lazyText :: Traversal' TL.Text Char
lazyText = lazyUnpacked . traversed
{-# INLINE [0] lazyText #-}

{-# RULES
"lazy text -> map"    lazyText = sets TL.map        :: ASetter' TL.Text Char;
"lazy text -> foldr"  lazyText = foldring TL.foldr  :: Getting (Endo r) TL.Text Char;
  #-}
