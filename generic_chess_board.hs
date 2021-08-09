#!/usr/bin/env stack
{- stack --resolver=lts-16.16 script
   --package diagrams-lib --package diagrams-svg
-}
module Main where

import Data.List (tails)
import Diagrams.Backend.SVG (renderSVG, B)
import Diagrams.Prelude

boardSq :: Colour Double -> Diagram B
boardSq c = square 1 # lineWidth none # fillColor c

chessBoard :: [Colour Double] -> Int -> Diagram B
chessBoard myColors n
    = vcat . map hcat . map (map boardSq)
    . take n . splitBy n . cycle
    $ myColors
    where splitBy size xs = let (i, t) = splitAt size xs
                            in i : splitBy size t

main = renderSVG "chess.svg" size $ chessBoard colors 8
    where
        size = mkSizeSpec2D (Just 400) (Just 400)
        colors = [red, white, blue, black, yellow]
