<<<<<<< HEAD
{-# LANGUAGE CPP, FlexibleContexts #-}

#ifdef GENERICS
{-# LANGUAGE DefaultSignatures #-}
#endif

-- |
-- Module:      Data.Aeson.Types.Class
-- Copyright:   (c) 2011-2013 Bryan O'Sullivan
=======
{-# LANGUAGE CPP, DefaultSignatures, FlexibleContexts #-}

-- |
-- Module:      Data.Aeson.Types.Class
-- Copyright:   (c) 2011-2015 Bryan O'Sullivan
>>>>>>> upstream/master
--              (c) 2011 MailRank, Inc.
-- License:     Apache
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable
--
-- Types for working with JSON data.

module Data.Aeson.Types.Class
    (
<<<<<<< HEAD
    -- * Type classes
    -- ** Core JSON classes
      FromJSON(..)
    , ToJSON(..)
#ifdef GENERICS
    -- ** Generic JSON classes
    , GFromJSON(..)
    , GToJSON(..)
    , genericToJSON
    , genericParseJSON
#endif
    ) where

import Data.Aeson.Types.Internal

#ifdef GENERICS
import GHC.Generics

-- | Class of generic representation types ('Rep') that can be converted to JSON.
=======
    -- * Core JSON classes
      FromJSON(..)
    , ToJSON(..)
    -- * Generic JSON classes
    , GFromJSON(..)
    , GToJSON(..)
    , genericToJSON
    , genericToEncoding
    , genericParseJSON
    -- * Object key-value pairs
    , KeyValue(..)
    -- * Functions needed for documentation
    , typeMismatch
    ) where

import Data.Aeson.Types.Internal
import Data.Text (Text)
import GHC.Generics (Generic, Rep, from, to)
import qualified Data.Aeson.Encode.Builder as E

-- | Class of generic representation types ('Rep') that can be converted to
-- JSON.
>>>>>>> upstream/master
class GToJSON f where
    -- | This method (applied to 'defaultOptions') is used as the
    -- default generic implementation of 'toJSON'.
    gToJSON :: Options -> f a -> Value

<<<<<<< HEAD
=======
    -- | This method (applied to 'defaultOptions') can be used as the
    -- default generic implementation of 'toEncoding'.
    gToEncoding :: Options -> f a -> Encoding

>>>>>>> upstream/master
-- | Class of generic representation types ('Rep') that can be converted from JSON.
class GFromJSON f where
    -- | This method (applied to 'defaultOptions') is used as the
    -- default generic implementation of 'parseJSON'.
    gParseJSON :: Options -> Value -> Parser (f a)

<<<<<<< HEAD
-- | A configurable generic JSON encoder. This function applied to
=======
-- | A configurable generic JSON creator. This function applied to
>>>>>>> upstream/master
-- 'defaultOptions' is used as the default for 'toJSON' when the type
-- is an instance of 'Generic'.
genericToJSON :: (Generic a, GToJSON (Rep a)) => Options -> a -> Value
genericToJSON opts = gToJSON opts . from

<<<<<<< HEAD
=======
-- | A configurable generic JSON encoder. This function applied to
-- 'defaultOptions' is used as the default for 'toEncoding' when the type
-- is an instance of 'Generic'.
genericToEncoding :: (Generic a, GToJSON (Rep a)) => Options -> a -> Encoding
genericToEncoding opts = gToEncoding opts . from

>>>>>>> upstream/master
-- | A configurable generic JSON decoder. This function applied to
-- 'defaultOptions' is used as the default for 'parseJSON' when the
-- type is an instance of 'Generic'.
genericParseJSON :: (Generic a, GFromJSON (Rep a)) => Options -> Value -> Parser a
genericParseJSON opts = fmap to . gParseJSON opts
<<<<<<< HEAD
#endif
=======
>>>>>>> upstream/master

-- | A type that can be converted to JSON.
--
-- An example type and instance:
--
<<<<<<< HEAD
-- @{-\# LANGUAGE OverloadedStrings #-}
=======
-- @
-- \-- Allow ourselves to write 'Text' literals.
-- {-\# LANGUAGE OverloadedStrings #-}
>>>>>>> upstream/master
--
-- data Coord = Coord { x :: Double, y :: Double }
--
-- instance ToJSON Coord where
--   toJSON (Coord x y) = 'object' [\"x\" '.=' x, \"y\" '.=' y]
<<<<<<< HEAD
-- @
--
-- Note the use of the @OverloadedStrings@ language extension which enables
-- 'Text' values to be written as string literals.
--
-- Instead of manually writing your 'ToJSON' instance, there are three options
-- to do it automatically:
--
-- * "Data.Aeson.TH" provides template-haskell functions which will derive an
-- instance at compile-time. The generated instance is optimized for your type
-- so will probably be more efficient than the following two options:
--
-- * "Data.Aeson.Generic" provides a generic @toJSON@ function that accepts any
-- type which is an instance of 'Data'.
--
-- * If your compiler has support for the @DeriveGeneric@ and
-- @DefaultSignatures@ language extensions (GHC 7.2 and newer),
-- @toJSON@ will have a default generic implementation.
--
-- To use the latter option, simply add a @deriving 'Generic'@ clause to your
-- datatype and declare a @ToJSON@ instance for your datatype without giving a
-- definition for @toJSON@.
--
-- For example the previous example can be simplified to just:
--
-- @{-\# LANGUAGE DeriveGeneric \#-}
--
-- import GHC.Generics
--
-- data Coord = Coord { x :: Double, y :: Double } deriving Generic
--
-- instance ToJSON Coord
-- @
--
-- Note that, instead of using @DefaultSignatures@, it's also possible
-- to parameterize the generic encoding using 'genericToJSON' applied
-- to your encoding/decoding 'Options':
--
-- @
-- instance ToJSON Coord where
--     toJSON = 'genericToJSON' 'defaultOptions'
-- @
class ToJSON a where
    toJSON   :: a -> Value

#ifdef GENERICS
    default toJSON :: (Generic a, GToJSON (Rep a)) => a -> Value
    toJSON = genericToJSON defaultOptions
#endif
=======
--
--   toEncoding (Coord x y) = 'pairs' (\"x\" '.=' x '<>' \"y\" '.=' y)
-- @
--
-- Instead of manually writing your 'ToJSON' instance, there are two options
-- to do it automatically:
--
-- * "Data.Aeson.TH" provides Template Haskell functions which will derive an
-- instance at compile time. The generated instance is optimized for your type
-- so will probably be more efficient than the following two options:
--
-- * The compiler can provide a default generic implementation for
-- 'toJSON'.
--
-- To use the second, simply add a @deriving 'Generic'@ clause to your
-- datatype and declare a 'ToJSON' instance for your datatype without giving
-- definitions for 'toJSON' or 'toEncoding'.
--
-- For example, the previous example can be simplified to a more
-- minimal instance:
--
-- @
-- {-\# LANGUAGE DeriveGeneric \#-}
--
-- import "GHC.Generics"
--
-- data Coord = Coord { x :: Double, y :: Double } deriving 'Generic'
--
-- instance ToJSON Coord where
--     toEncoding = 'genericToEncoding' 'defaultOptions'
-- @
--
-- Why do we provide an implementation for 'toEncoding' here?  The
-- 'toEncoding' function is a relatively new addition to this class.
-- To allow users of older versions of this library to upgrade without
-- having to edit all of their instances or encounter surprising
-- incompatibilities, the default implementation of 'toEncoding' uses
-- 'toJSON'.  This produces correct results, but since it performs an
-- intermediate conversion to a 'Value', it will be less efficient
-- than directly emitting an 'Encoding'.  Our one-liner definition of
-- 'toEncoding' above bypasses the intermediate 'Value'.
--
-- If @DefaultSignatures@ doesn't give exactly the results you want,
-- you can customize the generic encoding with only a tiny amount of
-- effort, using 'genericToJSON' and 'genericToEncoding' with your
-- preferred 'Options':
--
-- @
-- instance ToJSON Coord where
--     toJSON     = 'genericToJSON' 'defaultOptions'
--     toEncoding = 'genericToEncoding' 'defaultOptions'
-- @
class ToJSON a where
    -- | Convert a Haskell value to a JSON-friendly intermediate type.
    toJSON     :: a -> Value
    {-# MINIMAL toJSON #-}

    default toJSON :: (Generic a, GToJSON (Rep a)) => a -> Value
    toJSON = genericToJSON defaultOptions

    -- | Encode a Haskell value as JSON.
    --
    -- The default implementation of this method creates an
    -- intermediate 'Value' using 'toJSON'.  This provides
    -- source-level compatibility for people upgrading from older
    -- versions of this library, but obviously offers no performance
    -- advantage.
    --
    -- To benefit from direct encoding, you /must/ provide an
    -- implementation for this method.  The easiest way to do so is by
    -- having your types implement 'Generic' using the @DeriveGeneric@
    -- extension, and then have GHC generate a method body as follows.
    --
    -- @
    -- instance ToJSON Coord where
    --     toEncoding = 'genericToEncoding' 'defaultOptions'
    -- @

    toEncoding :: a -> Encoding
    toEncoding = Encoding . E.encodeToBuilder . toJSON
    {-# INLINE toEncoding #-}
>>>>>>> upstream/master

-- | A type that can be converted from JSON, with the possibility of
-- failure.
--
<<<<<<< HEAD
-- When writing an instance, use 'empty', 'mzero', or 'fail' to make a
-- conversion fail, e.g. if an 'Object' is missing a required key, or
-- the value is of the wrong type.
--
-- An example type and instance:
--
-- @{-\# LANGUAGE OverloadedStrings #-}
=======
-- In many cases, you can get the compiler to generate parsing code
-- for you (see below).  To begin, let's cover writing an instance by
-- hand.
--
-- There are various reasons a conversion could fail.  For example, an
-- 'Object' could be missing a required key, an 'Array' could be of
-- the wrong size, or a value could be of an incompatible type.
--
-- The basic ways to signal a failed conversion are as follows:
--
-- * 'empty' and 'mzero' work, but are terse and uninformative
--
-- * 'fail' yields a custom error message
--
-- * 'typeMismatch' produces an informative message for cases when the
-- value encountered is not of the expected type
--
-- An example type and instance:
--
-- @
-- \-- Allow ourselves to write 'Text' literals.
-- {-\# LANGUAGE OverloadedStrings #-}
>>>>>>> upstream/master
--
-- data Coord = Coord { x :: Double, y :: Double }
--
-- instance FromJSON Coord where
--   parseJSON ('Object' v) = Coord    '<$>'
--                          v '.:' \"x\" '<*>'
--                          v '.:' \"y\"
--
<<<<<<< HEAD
--   \-- A non-'Object' value is of the wrong type, so use 'mzero' to fail.
--   parseJSON _          = 'mzero'
-- @
--
-- Note the use of the @OverloadedStrings@ language extension which enables
-- 'Text' values to be written as string literals.
--
-- Instead of manually writing your 'FromJSON' instance, there are three options
-- to do it automatically:
--
-- * "Data.Aeson.TH" provides template-haskell functions which will derive an
-- instance at compile-time. The generated instance is optimized for your type
-- so will probably be more efficient than the following two options:
--
-- * "Data.Aeson.Generic" provides a generic @fromJSON@ function that parses to
-- any type which is an instance of 'Data'.
--
-- * If your compiler has support for the @DeriveGeneric@ and
-- @DefaultSignatures@ language extensions, @parseJSON@ will have a default
-- generic implementation.
--
-- To use this, simply add a @deriving 'Generic'@ clause to your datatype and
-- declare a @FromJSON@ instance for your datatype without giving a definition
-- for @parseJSON@.
--
-- For example the previous example can be simplified to just:
--
-- @{-\# LANGUAGE DeriveGeneric \#-}
--
-- import GHC.Generics
--
-- data Coord = Coord { x :: Double, y :: Double } deriving Generic
=======
--   \-- We do not expect a non-'Object' value here.
--   \-- We could use 'mzero' to fail, but 'typeMismatch'
--   \-- gives a much more informative error message.
--   parseJSON invalid    = 'typeMismatch' \"Coord\" invalid
-- @
--
-- Instead of manually writing your 'FromJSON' instance, there are two options
-- to do it automatically:
--
-- * "Data.Aeson.TH" provides Template Haskell functions which will derive an
-- instance at compile time. The generated instance is optimized for your type
-- so will probably be more efficient than the following two options:
--
-- * The compiler can provide a default generic implementation for
-- 'parseJSON'.
--
-- To use the second, simply add a @deriving 'Generic'@ clause to your
-- datatype and declare a 'FromJSON' instance for your datatype without giving
-- a definition for 'parseJSON'.
--
-- For example, the previous example can be simplified to just:
--
-- @
-- {-\# LANGUAGE DeriveGeneric \#-}
--
-- import "GHC.Generics"
--
-- data Coord = Coord { x :: Double, y :: Double } deriving 'Generic'
>>>>>>> upstream/master
--
-- instance FromJSON Coord
-- @
--
<<<<<<< HEAD
-- Note that, instead of using @DefaultSignatures@, it's also possible
-- to parameterize the generic decoding using 'genericParseJSON' applied
-- to your encoding/decoding 'Options':
=======
-- If @DefaultSignatures@ doesn't give exactly the results you want,
-- you can customize the generic decoding with only a tiny amount of
-- effort, using 'genericParseJSON' with your preferred 'Options':
>>>>>>> upstream/master
--
-- @
-- instance FromJSON Coord where
--     parseJSON = 'genericParseJSON' 'defaultOptions'
-- @

class FromJSON a where
    parseJSON :: Value -> Parser a

<<<<<<< HEAD
#ifdef GENERICS
    default parseJSON :: (Generic a, GFromJSON (Rep a)) => Value -> Parser a
    parseJSON = genericParseJSON defaultOptions
#endif
=======
    default parseJSON :: (Generic a, GFromJSON (Rep a)) => Value -> Parser a
    parseJSON = genericParseJSON defaultOptions

-- | A key-value pair for encoding a JSON object.
class KeyValue kv where
    (.=) :: ToJSON v => Text -> v -> kv
    infixr 8 .=

-- | Fail parsing due to a type mismatch, with a descriptive message.
--
-- Example usage:
--
-- @
-- instance FromJSON Coord where
--   parseJSON ('Object' v) = {- type matches, life is good -}
--   parseJSON wat        = 'typeMismatch' \"Coord\" wat
-- @
typeMismatch :: String -- ^ The name of the type you are trying to parse.
             -> Value  -- ^ The actual value encountered.
             -> Parser a
typeMismatch expected actual =
    fail $ "expected " ++ expected ++ ", encountered " ++ name
  where
    name = case actual of
             Object _ -> "Object"
             Array _  -> "Array"
             String _ -> "String"
             Number _ -> "Number"
             Bool _   -> "Boolean"
             Null     -> "Null"
>>>>>>> upstream/master
