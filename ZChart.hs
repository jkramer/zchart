
-- ZChart
-- Find the 10 most popular commands from your ZSH history and build a Google
-- Chart from it. Prints the charts URL to stdout and saves it as PNG in
-- "history.png".

import System.IO
import System.Environment

import Data.List
import Data.Char

import Control.Arrow

import Graphics.Google.Chart
import Data.ByteString.Char8 (unpack)
import Network.Download

import Data.Function


main = getEnv "HISTFILE" >>= readFile >>= googleChart . topTen . parse


parse = map (takeWhile (not . isSpace) . dropWhile (not . isAlpha)) . lines


topTen full = (length full, others : top)
    where
        others = ("others", sum $ map snd $ drop (length top) sorted)
        top = take 10 sorted
        sorted = sortBy (flip (compare `on` snd)) $ uniq full
        uniq = map (id &&& count) . nub
        count = length . flip elemIndices full


googleChart (total, values) =
    putStrLn uri >> openURI uri >>= writeFile "history.png" . check
    where
        check = either error unpack
        percents = map (flip (/) percent . fromIntegral . snd) values
        percent = (fromIntegral total) / 100.0
        uri = chartURL
            $ setSize 600 400
            $ setTitle "ZSH History Top 10"
            $ setData (encodeDataText [percents])
            $ setLabels (map fst values)
            $ newPieChart Pie2D
