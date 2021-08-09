#!/usr/bin/env stack
-- stack --resolver=lts-16.16 script --package diagrams-lib --package diagrams-rasterific --package vector --package unordered-containers --package aeson --package bytestring
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Arrow ((&&&))
import Control.Monad (forM_)
import Data.HashMap.Strict ((!))
import Data.List (tails)
import Diagrams.Backend.Rasterific (renderRasterific, B)

import qualified Data.Aeson as Json
import qualified Data.Aeson.Parser as Json
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector as Vector

import Diagrams.Prelude hiding (width)

parseJson :: BS.ByteString -> Either (Json.JSONPath, String) Json.Value
parseJson = Json.eitherDecodeStrictWith Json.json' pure

decodeDouble :: Json.Value -> Double
decodeDouble x = case Json.eitherDecode . Json.encode $ x of
    Left err -> error err
    Right v -> v

drawPointAt :: (Double, Double) -> Double -> Colour Double -> Diagram B
drawPointAt (x, y) radius color
    = fillColor color
    . lineColor color
    . translate (r2 (x, y))
    $ circle radius

renderSkeleton radius color = foldr1 atop . map draw . filter nonZero where
    draw coords = drawPointAt coords radius color
--     nonZero (0, 0) = False
    nonZero _ = True

data Scale = Scale
    { width :: Double
    , top :: Double
    , right :: Double
    } deriving (Eq, Show)

findScale :: [(Double, Double)] -> Scale
findScale coords =
    let (xs, ys) = unzip coords
        leftmost = minimum xs
        rightmost = maximum xs
        topmost = maximum ys
    in Scale
        { width = rightmost - leftmost
        , top = topmost
        , right = rightmost
        }

diagrams2d :: FilePath -> IO [(Diagram B, Scale)]
diagrams2d path
    = map (renderSkeleton 2.5 red &&& findScale)
    . map (extract . parse)
    . BS.lines
    <$> BS.readFile path
  where
    parse x = case parseJson x of
        Left err -> error $ show err
        Right val -> val
    extract (Json.Array a) = map toCoords $ Vector.toList a
    toCoords (Json.Object o) = (negate . decodeDouble $ o ! "u", negate . decodeDouble $ o ! "v")

diagrams3d :: FilePath -> IO [(Diagram B, Scale)]
diagrams3d path = map (renderSkeleton 0.01 blue &&& findScale) . map (extract . parse) . BS.lines <$> BS.readFile path where
    parse x = case parseJson x of
        Left err -> error $ show err
        Right val -> val
    extract (Json.Array a) = map toCoords $ Vector.toList a
    toCoords (Json.Object o) = (decodeDouble $ o ! "x", decodeDouble $ o ! "y")

scaleTo :: Scale -> Diagram B -> Scale -> Diagram B
scaleTo target d source =
    let koef = width target / width source
        delta_x = right target - (right source * koef)
        delta_y = top target - (top source * koef)
    in translate (r2 (delta_x, delta_y)) . scale koef $ d


main = do
    d2d <- diagrams2d "flat_skeletons.jsonl"
    d3d <- diagrams3d "skeletons.jsonl"
    let ds = zipWith (\(d1, s1) (d2, s2) -> d1 `atop` scaleTo s1 d2 s2) d2d d3d
    forM_ (zip [0,1..] ds) $ \(index, d) ->
        let name = "images/diagram_" ++ show index ++ ".png"
        in renderRasterific name size $ bg white d
    where size = mkSizeSpec2D (Just 800) (Just 800)#!/usr/bin/env stack
-- stack --resolver=lts-16.16 script --package diagrams-lib --package diagrams-rasterific --package vector --package unordered-containers --package aeson --package bytestring
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Arrow ((&&&))
import Control.Monad (forM_)
import Data.HashMap.Strict ((!))
import Data.List (tails)
import Diagrams.Backend.Rasterific (renderRasterific, B)

import qualified Data.Aeson as Json
import qualified Data.Aeson.Parser as Json
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector as Vector

import Diagrams.Prelude hiding (width)

parseJson :: BS.ByteString -> Either (Json.JSONPath, String) Json.Value
parseJson = Json.eitherDecodeStrictWith Json.json' pure

decodeDouble :: Json.Value -> Double
decodeDouble x = case Json.eitherDecode . Json.encode $ x of
    Left err -> error err
    Right v -> v

drawPointAt :: (Double, Double) -> Double -> Colour Double -> Diagram B
drawPointAt (x, y) radius color
    = fillColor color
    . lineColor color
    . translate (r2 (x, y))
    $ circle radius

renderSkeleton radius color = foldr1 atop . map draw . filter nonZero where
    draw coords = drawPointAt coords radius color
--     nonZero (0, 0) = False
    nonZero _ = True

data Scale = Scale
    { width :: Double
    , top :: Double
    , right :: Double
    } deriving (Eq, Show)

findScale :: [(Double, Double)] -> Scale
findScale coords =
    let (xs, ys) = unzip coords
        leftmost = minimum xs
        rightmost = maximum xs
        topmost = maximum ys
    in Scale
        { width = rightmost - leftmost
        , top = topmost
        , right = rightmost
        }

diagrams2d :: FilePath -> IO [(Diagram B, Scale)]
diagrams2d path
    = map (renderSkeleton 2.5 red &&& findScale)
    . map (extract . parse)
    . BS.lines
    <$> BS.readFile path
  where
    parse x = case parseJson x of
        Left err -> error $ show err
        Right val -> val
    extract (Json.Array a) = map toCoords $ Vector.toList a
    toCoords (Json.Object o) = (negate . decodeDouble $ o ! "u", negate . decodeDouble $ o ! "v")

diagrams3d :: FilePath -> IO [(Diagram B, Scale)]
diagrams3d path = map (renderSkeleton 0.01 blue &&& findScale) . map (extract . parse) . BS.lines <$> BS.readFile path where
    parse x = case parseJson x of
        Left err -> error $ show err
        Right val -> val
    extract (Json.Array a) = map toCoords $ Vector.toList a
    toCoords (Json.Object o) = (decodeDouble $ o ! "x", decodeDouble $ o ! "y")

scaleTo :: Scale -> Diagram B -> Scale -> Diagram B
scaleTo target d source =
    let koef = width target / width source
        delta_x = right target - (right source * koef)
        delta_y = top target - (top source * koef)
    in translate (r2 (delta_x, delta_y)) . scale koef $ d


main = do
    d2d <- diagrams2d "flat_skeletons.jsonl"
    d3d <- diagrams3d "skeletons.jsonl"
    let ds = zipWith (\(d1, s1) (d2, s2) -> d1 `atop` scaleTo s1 d2 s2) d2d d3d
    forM_ (zip [0,1..] ds) $ \(index, d) ->
        let name = "images/diagram_" ++ show index ++ ".png"
        in renderRasterific name size $ bg white d
    where size = mkSizeSpec2D (Just 800) (Just 800)
