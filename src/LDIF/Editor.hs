------------------------------------------------------------------------
-- |
-- Module      : LDIF.Editor
-- Copyright   : Jonas Juselius 2015
-- License     : BSD3
--
-- Maintainer  : Jonas Juselius <jonas.juselius@gmail.com>
-- Stability   : provisional
-- Portability : non-portable
--
-- The LDIF.Editor module implements a rule-based editor for
-- rewriting, deleteing and inserting fields into LDIF data.
-- The editor module implements depth control for recursive data
-- structures, both for efficiency and to make it easier to specify
-- general editing rules, i.e. rules which apply only at the e.g.
-- attribute level.
------------------------------------------------------------------------
module LDIF.Editor (module M) where

import LDIF.Editor.Edit as M
import LDIF.Editor.Rules as M
