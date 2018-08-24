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
        weight = parseRecords cursor "HKQuantityTypeIdentifierBodyMass"
        fatPercent = parseRecords cursor "HKQuantityTypeIdentifierBodyFatPercentage"

    ByteString.writeFile "weight.csv" $ encode $ ("Date", "Weight (LBS)") : weight
    ByteString.writeFile "fat_percent.csv" $ encode $ ("Date", "Fat Percentage") : fatPercent

parseRecords :: Cursor -> T.Text -> [(T.Text, T.Text)]
parseRecords c attr =
  L.sortOn fst
    $ unique
    $ c
    $// element "Record"
    >=> attributeIs "type" attr
    &| parserDateAndValue


parserDateAndValue :: Cursor -> (T.Text, T.Text)
parserDateAndValue c = do
  let v = attribute "value" c
      d = attribute "startDate" c
  (parseDate $ head d, head v)

parseDate :: T.Text -> T.Text
parseDate =
  T.takeWhile (/= ' ')

unique :: Eq a => [(a, b)] -> [(a, b)]
unique [] = []
unique (x : xs) = x : unique (L.filter (\x2 -> fst x /= fst x2) xs)
