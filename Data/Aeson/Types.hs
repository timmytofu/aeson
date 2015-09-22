<<<<<<< HEAD
{-# LANGUAGE CPP #-}

-- |
-- Module:      Data.Aeson.Types
-- Copyright:   (c) 2011, 2012 Bryan O'Sullivan
=======
-- |
-- Module:      Data.Aeson.Types
-- Copyright:   (c) 2011-2015 Bryan O'Sullivan
>>>>>>> upstream/master
--              (c) 2011 MailRank, Inc.
-- License:     Apache
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable
--
-- Types for working with JSON data.

module Data.Aeson.Types
    (
    -- * Core JSON types
      Value(..)
<<<<<<< HEAD
=======
    , Encoding
    , fromEncoding
    , Series
>>>>>>> upstream/master
    , Array
    , emptyArray
    , Pair
    , Object
    , emptyObject
    -- * Convenience types and functions
    , DotNetTime(..)
    , typeMismatch
    -- * Type conversion
    , Parser
    , Result(..)
    , FromJSON(..)
    , fromJSON
    , parse
    , parseEither
    , parseMaybe
    , ToJSON(..)
<<<<<<< HEAD
    , modifyFailure

#ifdef GENERICS
=======
    , KeyValue(..)
    , modifyFailure

>>>>>>> upstream/master
    -- ** Generic JSON classes
    , GFromJSON(..)
    , GToJSON(..)
    , genericToJSON
<<<<<<< HEAD
    , genericParseJSON
#endif
=======
    , genericToEncoding
    , genericParseJSON
>>>>>>> upstream/master

    -- * Inspecting @'Value's@
    , withObject
    , withText
    , withArray
    , withNumber
    , withScientific
    , withBool

<<<<<<< HEAD
    -- * Constructors and accessors
    , (.=)
=======
    , pairs
    , foldable
>>>>>>> upstream/master
    , (.:)
    , (.:?)
    , (.!=)
    , object

    -- * Generic and TH encoding configuration
    , Options(..)
    , SumEncoding(..)
    , camelTo
<<<<<<< HEAD
=======
    , camelTo2
>>>>>>> upstream/master
    , defaultOptions
    , defaultTaggedObject
    ) where

<<<<<<< HEAD
import Data.Aeson.Types.Instances
import Data.Aeson.Types.Internal

#ifdef GENERICS
import Data.Aeson.Types.Generic ()
#endif
=======
import Data.Aeson.Encode.Functions (foldable, pairs)
import Data.Aeson.Types.Generic ()
import Data.Aeson.Types.Instances
import Data.Aeson.Types.Internal
>>>>>>> upstream/master
