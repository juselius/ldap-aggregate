--------------------------------------------------------------------------------
-- |
-- Module      :  LDIF
-- Copyright   :  (c) Jonas Juselius 2015
-- License     :  BSD3
--
-- Maintainer  :  Jonas Juselius <jonas.juselius@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
-- The LDIF module provides a basic LDIF parser. In addition it provides
-- the follwing features:
--
-- * The Diff module produces change LDIF from the difference of two sets of
--   LDIF entries
-- * Apply applies change LDIF to entries, generatiting update LDIF entries
-- * The Edit module provides the capability to delete, rewrite and insert
--   entries and attributes in LDIF records
-- * The LDAP module provides helpers to convert LDIF data types to LDAP
--   data and back, making it easy to work with the LDAP server module.
--
-- The purpose of the Edit module is to filter out and rewrite entries,
-- attributes and values before taking the difference between sets of entries.
--------------------------------------------------------------------------------

module LDIF (module M) where

import LDIF.Types as M
import LDIF.Parser as M
import LDIF.Diff as M
import LDIF.Apply as M
import LDIF.LDAP as M
