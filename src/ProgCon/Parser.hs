{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Generated with http://json-to-haskell.chrispenner.ca/
module ProgCon.Parser where

import ProgCon.Syntax

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, (.:), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (prependFailure, typeMismatch)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import Data.Vector.Unboxed qualified as UV
import Data.Vector.Unboxed qualified as VU

loadJSON :: FromJSON a => FilePath -> IO a
loadJSON fp =
    Aeson.eitherDecodeFileStrict fp >>= \case
        Right m -> pure m
        Left e -> error $ fp <> ": aeson error: " <> e

instance ToJSON Solution where
    toJSON Solution = Null

instance FromJSON Solution where
    parseJSON Null = pure Solution
    parseJSON invalid = do
        prependFailure
            "parsing Solution failed, "
            (typeMismatch "Object" invalid)

instance FromJSON Problem where
    parseJSON Null = pure Problem
    parseJSON invalid = do
        prependFailure
            "parsing Problem failed, "
            (typeMismatch "Object" invalid)
