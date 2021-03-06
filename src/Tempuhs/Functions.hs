{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

{- |
Module      :  $Header$
Description :  tempuhs functions.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Tempuhs.Functions where

import Data.Time.Clock
  (
  DiffTime,
  UTCTime (UTCTime),
  secondsToDiffTime,
  )
import Data.Time.Clock.TAI
  (
  AbsoluteTime,
  LeapSecondTable,
  diffAbsoluteTime,
  taiEpoch,
  utcToTAITime,
  )

import Plailude
import Tempuhs.Types
  (
  ProperTime,
  TimeRange,
  )

second :: ProperTime
-- | 'second' is one SI second of proper time.
second = 1

minute :: ProperTime
-- | 'minute' is exactly 60 'second's of proper time.
minute = 60 * second

hour :: ProperTime
-- | 'hour' is exactly 60 'minute's of proper time.
hour = 60 * minute

day :: ProperTime
-- | 'day' is exactly 24 'hour's of proper time.
day = 24 * hour

annum :: ProperTime
-- | 'annum' is one astronomical Julian year, i.e. exactly 365.25 'day's.
annum = 365.25 * day

expRange :: ProperTime -> Double -> ProperTime -> TimeRange
-- | 'expRange' gives a 'TimeRange' from an exponentiation with base ten and
-- an uncertainty applied to the exponent.
expRange factor e uncertainty =
  let f o = factor * 10 ** (e `o` uncertainty) in (f (-), f (+))

linRange :: Double -> Double -> ProperTime -> TimeRange
-- | 'linRange' gives a 'TimeRange' from a value with linear uncertainty.
linRange val uncertainty unit =
  let f o = unit * (val `o` uncertainty) in (f (-), f (+))

taiToJ2000 :: AbsoluteTime -> ProperTime
-- | 'taiToJ2000' converts TAI time to 'ProperTime' with J2000 as the epoch.
taiToJ2000 t =
  (realToFrac(diffAbsoluteTime t taiEpoch) + 32.184) * second - 51544.5 * day

utcToJ2000 :: LeapSecondTable -> UTCTime -> ProperTime
-- | 'utcToJ2000' converts 'UTCTime' to 'ProperTime' with J2000 as the epoch.
utcToJ2000 lst = taiToJ2000 . utcToTAITime lst

hmsToDiffTime :: Hour h -> Minute m -> Second s -> DiffTime
-- | 'hmsToDiffTime' converts time of day in hours, minutes and seconds to the
-- time from midnight as a 'DiffTime'.
hmsToDiffTime = (secondsToDiffTime . timeVal) .:. asSeconds

parseUTC :: Year y -> Month mo -> Day d
         -> Hour h -> Minute m -> Second s -> UTCTime
-- | 'parseUTC' takes year, month and day in the Gregorian calendar, along
-- with hours, minutes and seconds in UTC, and returns a 'UTCTime'.
parseUTC y mo d h m s = fromGregorian y mo d `UTCTime` hmsToDiffTime h m s
