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
  UniqueClock             name
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
Permissions
  timespan                TimespanId
  role                    RoleId
  own                     Bool
  read                    Bool
  write                   Bool
  share                   Bool
  UniquePermissions       timespan role
  deriving Show
Role json
  name                    Text
  namespace               UserId
  UniqueRole              namespace name
  rubbish                 UTCTime         Maybe
  deriving Show
User json
  name                    Text
  UniqueUser              name
  rubbish                 UTCTime         Maybe
  deriving Show
UserRole json
  user                    UserId
  role                    RoleId
  UniqueUserRole          user role
  rubbish                 UTCTime         Maybe
  deriving Show
|]

-- Make typeclasses and lenses for every field.
makeFields ''Timespan
makeFields ''User
makeFields ''Role
