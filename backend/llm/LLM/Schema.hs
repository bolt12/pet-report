{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Bridging a Haskell type's codec to the wire schema a model is constrained by. The
-- decoder and the @response_format@ schema come from the same 'HasCodec' instance, so what
-- the model is told to produce cannot drift from what the code can parse.
module LLM.Schema
  ( codecSchema
  , jsonSchemaFormat
  ) where

import           Autodocodec        (HasCodec)
import           Autodocodec.Schema (jsonSchemaViaCodec)
import           Data.Aeson         (Value, object, toJSON, (.=))
import           Data.Text          (Text)

-- | The JSON Schema of a type's 'HasCodec' instance. Fix @a@ at the use site, e.g.
-- @codecSchema \@Scene@.
codecSchema :: forall a. (HasCodec a) => Value
codecSchema = toJSON (jsonSchemaViaCodec @a)

-- | Wrap a JSON Schema as an OpenAI-style @response_format@ of type @json_schema@ under
-- @name@. Give it a codec-derived schema, e.g. @codecSchema \@Scene@.
jsonSchemaFormat :: Text -> Value -> Value
jsonSchemaFormat name sch =
  object
    [ "type" .= ("json_schema" :: Text)
    , "json_schema" .= object ["name" .= name, "schema" .= sch]
    ]
