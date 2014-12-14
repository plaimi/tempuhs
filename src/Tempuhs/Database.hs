{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

{- |
Module      :  $Header$
Description :  tempuhs database.
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Tempuhs.Database where

import Control.Lens
  (
  makeFields,
  )
import Data.Text
  (
  Text,
  )
import Data.Time.Clock
  (
  UTCTime,
  )
import Database.Persist.TH
  (
  mkMigrate,
  mkPersist,
  persistLowerCase,
  share,
  )

import Tempuhs.Internal
  (
  tempuhsSettings,
  )
import Tempuhs.Types
  (
  ProperTime,
  Weight,
  )

share [mkPersist tempuhsSettings, mkMigrate "migrateAll"] [persistLowerCase|
Clock json
  name                    Text
  rubbish                 UTCTime         Maybe
  UniqueClock             name
  deriving Show
Permissionset json
  timespan                TimespanId
  role                    RoleId
  own                     Bool
  read                    Bool
  write                   Bool
  rubbish                 UTCTime         Maybe
  UniquePermissionset     timespan role
  deriving Show
Role json
  name                    Text
  namespace               UserId
  rubbish                 UTCTime         Maybe
  UniqueRole              namespace name
  deriving Show
Timespan json
  parent                  TimespanId      Maybe
  clock                   ClockId
  beginMin                ProperTime
  beginMax                ProperTime
  endMin                  ProperTime
  endMax                  ProperTime
  weight                  Weight
  rubbish                 UTCTime         Maybe
  deriving Show
TimespanAttribute json
  timespan                TimespanId
  name                    Text
  value                   Text
  UniqueTimespanAttribute timespan name
  deriving Show
User json
  name                    Text
  rubbish                 UTCTime         Maybe
  UniqueUser              name
  deriving Show
UserAttribute json
  user                    UserId
  name                    Text
  value                   Text
  UniqueUserAttribute     user name
  deriving Show
UserRole json
  user                    UserId
  role                    RoleId
  rubbish                 UTCTime         Maybe
  UniqueUserRole          user role
  deriving Show
|]

-- Make typeclasses and lenses for every field.
makeFields ''Clock
makeFields ''Permissionset
makeFields ''Role
makeFields ''Timespan
makeFields ''User
