#!/usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (p: with p; [Chart Chart-cairo])" -i runghc

import Graphics.Rendering.Chart.Easy
import Control.Monad (forM_)
import Graphics.Rendering.Chart.Backend.Cairo (toFile, fo_size)
import System.Directory (listDirectory)

main = do
    files <- listDirectory "coordinator-measurements"
    measurements <- traverse (fmap (drop 1) . readCsv . ("coordinator-measurements/" <>)) files -- drop csv header
    let fopts = def & fo_size .~ (800, 800)
    toFile fopts "coordinator-measurements.png" $ do
        layout_all_font_styles . font_size .= 14
        layout_title .= "Measurements by coordinator (avg)"
        layout_x_axis . laxis_title .= "ping (ms)"
        layout_y_axis . laxis_title .= "measured time (ms)"
        forM_ (zip measurements files) $ \(datas, filename) ->
            let points = [(read ping :: Int, read time :: Int) | [ping, time] <- datas]
                points :: [(Int, Int)]
            in plot $ line filename [points]

readCsv :: FilePath -> IO [[String]]
readCsv path = map (splitBy ',') . lines <$> readFile path
    where
        splitBy _ [] = []
        splitBy char xs =
            let (i, t) = span (/= char) xs
            in i : splitBy char (drop 1 t)
