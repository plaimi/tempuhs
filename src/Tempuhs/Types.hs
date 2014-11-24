{- |
Module      :  $Header$
Description :  Types used by tempuhs.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Tempuhs.Types where

-- | A 'ProperTime' is time measured by a 'Clock'.
type ProperTime = Double
-- | A 'TimeRange' represents a range between two 'ProperTime' endpoints.
type TimeRange = (ProperTime, ProperTime)
-- | A 'Weight' is used to signify the relative significance of a 'Timespan'.
type Weight = Double
