{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( healthXmlToCsv
    ) where

import qualified Data.ByteString.Lazy as ByteString
import Data.Csv
import Prelude hiding (readFile)
import Text.XML
import Text.XML.Cursor
import qualified Data.List as L
import qualified Data.Text as T

healthXmlToCsv :: String -> IO ()
healthXmlToCsv file = do
    doc <- readFile def file
    let cursor = fromDocument doc
        weight = cursor $// element "Record"
                     >=> attributeIs "type" "HKQuantityTypeIdentifierBodyMass"
                     &| parserDateAndValue
        fatPercent = cursor $// element "Record"
                     >=> attributeIs "type" "HKQuantityTypeIdentifierBodyFatPercentage"
                     &| parserDateAndValue
    ByteString.writeFile "weight.csv" $ encode $ ("Date", "Weight (LBS)") : weight
    ByteString.writeFile "fat_percent.csv" $ encode $ ("Date", "Fat Percentage") : fatPercent

parserDateAndValue :: Cursor -> (T.Text, T.Text)
parserDateAndValue n = do
  let v = attribute "value" n
      d = attribute "startDate" n
  (head d, head v)
