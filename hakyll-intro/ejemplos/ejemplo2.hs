{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Hakyll

main :: IO ()
main = hakyll siteGenerator

siteGenerator :: Rules ()
siteGenerator = match "static/css/*.css" compileCss

compileCss :: Rules ()
compileCss = compile copyFileCompiler
