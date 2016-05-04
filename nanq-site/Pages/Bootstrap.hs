{-# LANGUAGE OverloadedStrings #-}

module Pages.Bootstrap
       ( -- * Overrides for Bootstrap 3
         containerFluid_
       , row_
         -- ** Columns
       , col3_
       , col4_
       , col5_
       , col6_
       , col8_
       , col10_
       , col12_
         -- * Misc.
       , center_
       ) where

import Lucid

---

-- | A fluid grid container.
containerFluid_ :: Term arg result => arg -> result
containerFluid_ = termWith "div" [class_ " container-fluid "]

-- | A grid row.
row_ :: Term arg result => arg -> result
row_ = termWith "div" [class_ " row "]

-- | col-md-3
col3_ :: Term arg result => arg -> result
col3_ = termWith "div" [class_ " col-md-3 "]

-- | col-md-4
col4_ :: Term arg result => arg -> result
col4_ = termWith "div" [class_ " col-md-4 "]

-- | col-md-5
col5_ :: Term arg result => arg -> result
col5_ = termWith "div" [class_ " col-md-5 "]

-- | col-md-6
col6_ :: Term arg result => arg -> result
col6_ = termWith "div" [class_ " col-md-6 "]

-- | col-md-8
col8_ :: Term arg result => arg -> result
col8_ = termWith "div" [class_ " col-md-8 "]

-- | col-md-10
col10_ :: Term arg result => arg -> result
col10_ = termWith "div" [class_ " col-md-10 "]

-- | col-md-12
col12_ :: Term arg result => arg -> result
col12_ = termWith "div" [class_ " col-md-12 "]

-- | The @<center>@ tag isn't supported in HTML5. This centers both
-- divs and plain text.
center_ :: Term arg result => arg -> result
center_ = termWith "div"
  [style_ "display:flex;justify-content:center;align-items:center;"]
