<<<<<<< HEAD
{-# Language CPP, TemplateHaskell #-}
=======
{-# Language TemplateHaskell #-}
>>>>>>> upstream/master

module Encoders where

import Data.Aeson.TH
import Data.Aeson.Types
import Options
import Types

--------------------------------------------------------------------------------
-- Nullary encoders/decoders
--------------------------------------------------------------------------------

thNullaryToJSONString :: Nullary -> Value
thNullaryToJSONString = $(mkToJSON defaultOptions ''Nullary)

<<<<<<< HEAD
=======
thNullaryToEncodingString :: Nullary -> Encoding
thNullaryToEncodingString = $(mkToEncoding defaultOptions ''Nullary)

>>>>>>> upstream/master
thNullaryParseJSONString :: Value -> Parser Nullary
thNullaryParseJSONString = $(mkParseJSON defaultOptions ''Nullary)


thNullaryToJSON2ElemArray :: Nullary -> Value
thNullaryToJSON2ElemArray = $(mkToJSON opts2ElemArray ''Nullary)

<<<<<<< HEAD
=======
thNullaryToEncoding2ElemArray :: Nullary -> Encoding
thNullaryToEncoding2ElemArray = $(mkToEncoding opts2ElemArray ''Nullary)

>>>>>>> upstream/master
thNullaryParseJSON2ElemArray :: Value -> Parser Nullary
thNullaryParseJSON2ElemArray = $(mkParseJSON opts2ElemArray ''Nullary)


thNullaryToJSONTaggedObject :: Nullary -> Value
thNullaryToJSONTaggedObject = $(mkToJSON optsTaggedObject ''Nullary)

<<<<<<< HEAD
=======
thNullaryToEncodingTaggedObject :: Nullary -> Encoding
thNullaryToEncodingTaggedObject = $(mkToEncoding optsTaggedObject ''Nullary)

>>>>>>> upstream/master
thNullaryParseJSONTaggedObject :: Value -> Parser Nullary
thNullaryParseJSONTaggedObject = $(mkParseJSON optsTaggedObject ''Nullary)


thNullaryToJSONObjectWithSingleField :: Nullary -> Value
<<<<<<< HEAD
thNullaryToJSONObjectWithSingleField = $(mkToJSON optsObjectWithSingleField ''Nullary)
=======
thNullaryToJSONObjectWithSingleField =
  $(mkToJSON optsObjectWithSingleField ''Nullary)

thNullaryToEncodingObjectWithSingleField :: Nullary -> Encoding
thNullaryToEncodingObjectWithSingleField =
  $(mkToEncoding optsObjectWithSingleField ''Nullary)
>>>>>>> upstream/master

thNullaryParseJSONObjectWithSingleField :: Value -> Parser Nullary
thNullaryParseJSONObjectWithSingleField = $(mkParseJSON optsObjectWithSingleField ''Nullary)

<<<<<<< HEAD
#ifdef GHC_GENERICS
=======
>>>>>>> upstream/master
gNullaryToJSONString :: Nullary -> Value
gNullaryToJSONString = genericToJSON defaultOptions

gNullaryParseJSONString :: Value -> Parser Nullary
gNullaryParseJSONString = genericParseJSON defaultOptions


gNullaryToJSON2ElemArray :: Nullary -> Value
gNullaryToJSON2ElemArray = genericToJSON opts2ElemArray

gNullaryParseJSON2ElemArray :: Value -> Parser Nullary
gNullaryParseJSON2ElemArray = genericParseJSON opts2ElemArray


gNullaryToJSONTaggedObject :: Nullary -> Value
gNullaryToJSONTaggedObject = genericToJSON optsTaggedObject

gNullaryParseJSONTaggedObject :: Value -> Parser Nullary
gNullaryParseJSONTaggedObject = genericParseJSON optsTaggedObject


gNullaryToJSONObjectWithSingleField :: Nullary -> Value
gNullaryToJSONObjectWithSingleField = genericToJSON optsObjectWithSingleField

gNullaryParseJSONObjectWithSingleField :: Value -> Parser Nullary
gNullaryParseJSONObjectWithSingleField = genericParseJSON optsObjectWithSingleField
<<<<<<< HEAD
#endif
=======
>>>>>>> upstream/master


--------------------------------------------------------------------------------
-- SomeType encoders/decoders
--------------------------------------------------------------------------------

<<<<<<< HEAD
type SomeTypeToJSON = SomeType Int -> Value

thSomeTypeToJSON2ElemArray :: ToJSON a => SomeType a -> Value
thSomeTypeToJSON2ElemArray = $(mkToJSON opts2ElemArray ''SomeType)

thSomeTypeParseJSON2ElemArray :: FromJSON a => Value -> Parser (SomeType a)
thSomeTypeParseJSON2ElemArray = $(mkParseJSON opts2ElemArray ''SomeType)


thSomeTypeToJSONTaggedObject :: ToJSON a => SomeType a -> Value
thSomeTypeToJSONTaggedObject = $(mkToJSON optsTaggedObject ''SomeType)

thSomeTypeParseJSONTaggedObject :: FromJSON a => Value -> Parser (SomeType a)
thSomeTypeParseJSONTaggedObject = $(mkParseJSON optsTaggedObject ''SomeType)


thSomeTypeToJSONObjectWithSingleField :: ToJSON a => SomeType a -> Value
thSomeTypeToJSONObjectWithSingleField = $(mkToJSON optsObjectWithSingleField ''SomeType)

thSomeTypeParseJSONObjectWithSingleField :: FromJSON a => Value -> Parser (SomeType a)
thSomeTypeParseJSONObjectWithSingleField = $(mkParseJSON optsObjectWithSingleField ''SomeType)


#ifdef GHC_GENERICS
gSomeTypeToJSON2ElemArray :: ToJSON a => SomeType a -> Value
gSomeTypeToJSON2ElemArray = genericToJSON opts2ElemArray

gSomeTypeParseJSON2ElemArray :: FromJSON a => Value -> Parser (SomeType a)
gSomeTypeParseJSON2ElemArray = genericParseJSON opts2ElemArray


gSomeTypeToJSONTaggedObject :: ToJSON a => SomeType a -> Value
gSomeTypeToJSONTaggedObject = genericToJSON optsTaggedObject

gSomeTypeParseJSONTaggedObject :: FromJSON a => Value -> Parser (SomeType a)
gSomeTypeParseJSONTaggedObject = genericParseJSON optsTaggedObject


gSomeTypeToJSONObjectWithSingleField :: ToJSON a => SomeType a -> Value
gSomeTypeToJSONObjectWithSingleField = genericToJSON optsObjectWithSingleField

gSomeTypeParseJSONObjectWithSingleField :: FromJSON a => Value -> Parser (SomeType a)
gSomeTypeParseJSONObjectWithSingleField = genericParseJSON optsObjectWithSingleField
#endif
=======
thSomeTypeToJSON2ElemArray :: SomeType Int -> Value
thSomeTypeToJSON2ElemArray = $(mkToJSON opts2ElemArray ''SomeType)

thSomeTypeToEncoding2ElemArray :: SomeType Int -> Encoding
thSomeTypeToEncoding2ElemArray = $(mkToEncoding opts2ElemArray ''SomeType)

thSomeTypeParseJSON2ElemArray :: Value -> Parser (SomeType Int)
thSomeTypeParseJSON2ElemArray = $(mkParseJSON opts2ElemArray ''SomeType)


thSomeTypeToJSONTaggedObject :: SomeType Int -> Value
thSomeTypeToJSONTaggedObject = $(mkToJSON optsTaggedObject ''SomeType)

thSomeTypeToEncodingTaggedObject :: SomeType Int -> Encoding
thSomeTypeToEncodingTaggedObject = $(mkToEncoding optsTaggedObject ''SomeType)

thSomeTypeParseJSONTaggedObject :: Value -> Parser (SomeType Int)
thSomeTypeParseJSONTaggedObject = $(mkParseJSON optsTaggedObject ''SomeType)


thSomeTypeToJSONObjectWithSingleField :: SomeType Int -> Value
thSomeTypeToJSONObjectWithSingleField =
  $(mkToJSON optsObjectWithSingleField ''SomeType)

thSomeTypeToEncodingObjectWithSingleField :: SomeType Int -> Encoding
thSomeTypeToEncodingObjectWithSingleField =
  $(mkToEncoding optsObjectWithSingleField ''SomeType)

thSomeTypeParseJSONObjectWithSingleField :: Value -> Parser (SomeType Int)
thSomeTypeParseJSONObjectWithSingleField =
  $(mkParseJSON optsObjectWithSingleField ''SomeType)


gSomeTypeToJSON2ElemArray :: SomeType Int -> Value
gSomeTypeToJSON2ElemArray = genericToJSON opts2ElemArray

gSomeTypeParseJSON2ElemArray :: Value -> Parser (SomeType Int)
gSomeTypeParseJSON2ElemArray = genericParseJSON opts2ElemArray


gSomeTypeToJSONTaggedObject :: SomeType Int -> Value
gSomeTypeToJSONTaggedObject = genericToJSON optsTaggedObject

gSomeTypeParseJSONTaggedObject :: Value -> Parser (SomeType Int)
gSomeTypeParseJSONTaggedObject = genericParseJSON optsTaggedObject


gSomeTypeToJSONObjectWithSingleField :: SomeType Int -> Value
gSomeTypeToJSONObjectWithSingleField = genericToJSON optsObjectWithSingleField

gSomeTypeParseJSONObjectWithSingleField :: Value -> Parser (SomeType Int)
gSomeTypeParseJSONObjectWithSingleField = genericParseJSON optsObjectWithSingleField


--------------------------------------------------------------------------------
-- Approx encoders/decoders
--------------------------------------------------------------------------------

thApproxToJSONUnwrap :: Approx String -> Value
thApproxToJSONUnwrap = $(mkToJSON optsUnwrapUnaryRecords ''Approx)

thApproxToEncodingUnwrap :: Approx String -> Encoding
thApproxToEncodingUnwrap = $(mkToEncoding optsUnwrapUnaryRecords ''Approx)

thApproxParseJSONUnwrap :: Value -> Parser (Approx String)
thApproxParseJSONUnwrap = $(mkParseJSON optsUnwrapUnaryRecords ''Approx)


thApproxToJSONDefault :: Approx String -> Value
thApproxToJSONDefault = $(mkToJSON defaultOptions ''Approx)

thApproxToEncodingDefault :: Approx String -> Encoding
thApproxToEncodingDefault = $(mkToEncoding defaultOptions ''Approx)

thApproxParseJSONDefault :: Value -> Parser (Approx String)
thApproxParseJSONDefault = $(mkParseJSON defaultOptions ''Approx)

gApproxToJSONUnwrap :: Approx String -> Value
gApproxToJSONUnwrap = genericToJSON optsUnwrapUnaryRecords

gApproxParseJSONUnwrap :: Value -> Parser (Approx String)
gApproxParseJSONUnwrap = genericParseJSON optsUnwrapUnaryRecords


gApproxToJSONDefault :: Approx String -> Value
gApproxToJSONDefault = genericToJSON defaultOptions

gApproxParseJSONDefault :: Value -> Parser (Approx String)
gApproxParseJSONDefault = genericParseJSON defaultOptions
>>>>>>> upstream/master
