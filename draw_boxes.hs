#!/usr/bin/env stack
-- stack --resolver=lts-16.16 script --package diagrams-lib --package diagrams-rasterific
{-# LANGUAGE LambdaCase #-}

import Diagrams.Backend.Rasterific (renderRasterific, B)

import Diagrams.Prelude

mainDiagram :: DImage Double Embedded -> (Double, Double, Double, Double) -> Diagram B
mainDiagram background (ymin, xmin, ymax, xmax) = box `atop` preparedBackground where
    box = showOrigin $
        let
          w = xmax - xmin
          h = ymax - ymin
          centerX = (xmax + xmin) / 2
          centerY = (ymax + ymin) / 2
        in translate (r2 (centerX, centerY))
         . lineColor red
         $ rect w h
    preparedBackground =
        translate (r2 (0.5, 0.5))
        . scaleToX 1 . scaleToY 1
        $ image background

main = do
    background <- loadImageEmb "data/josh.png" >>= \case
        Left msg -> error msg
        Right img -> pure img
    let coords = (0.283329, 0.734205, 0.459308, 0.815661)
    renderRasterific "result.png" size $ mainDiagram background coords
    where
        size = mkSizeSpec2D (Just 1920) Nothing
