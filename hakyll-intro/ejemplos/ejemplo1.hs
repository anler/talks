module Main where

import           Hakyll

main :: IO ()
main = hakyll siteGenerator

siteGenerator :: Rules ()
siteGenerator = return ()
