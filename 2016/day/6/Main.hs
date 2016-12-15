import Data.List
import Data.Ord

--- Day 6: Signals and Noise ---
freq1 = head . maximumBy (comparing length) . group . sort

main1 = readFile "input.txt" >>= print . map freq1 . transpose . lines

--- Part Two ---
freq2 = head . minimumBy (comparing length) . group . sort

main2 = readFile "input.txt" >>= print . map freq2 . transpose . lines

main = main2
