{-# LANGUAGE TemplateHaskell   #-}

{- |
Module      :  $Header$
Description :  Internals for tempuhs
Copyright   :  (c) plaimi 2014
License     :  AGPL-3

Maintainer  :  tempuhs@plaimi.net
-} module Tempuhs.Internal where

import Database.Persist.Class
  (
  entityIdFromJSON,
  entityIdToJSON,
  )
import Database.Persist.TH
  (
  EntityJSON (EntityJSON),
  MkPersistSettings,
  entityFromJSON,
  entityToJSON,
  mpsEntityJSON,
  sqlSettings,
  )

tempuhsSettings :: MkPersistSettings
-- | 'MkPersistSettings' for tempuhs entities.
tempuhsSettings = sqlSettings
  { mpsEntityJSON = Just EntityJSON { entityToJSON = 'entityIdToJSON
                                    , entityFromJSON = 'entityIdFromJSON } }
