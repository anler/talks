{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Hakyll
import           System.Process

main :: IO ()
main = hakyll siteGenerator

siteGenerator :: Rules ()
siteGenerator =
  match "static/css/*.css" compileCss >>
  match "static/**.less" compileLess

compileCss :: Rules ()
compileCss =
  compile copyFileCompiler >>
  route idRoute

compileLess :: Rules ()
compileLess = do
  compile $ do
    item <- lessCompiler
    return $ compressCss <$> item
  route $ setExtension "css"

lessCompiler :: Compiler (Item String)
lessCompiler = withItemBody callLessc =<< getResourceBody
  where
    callLessc less = do
      unsafeCompiler $ do
        let cmd = "node_modules/less/bin/lessc"
        readProcess cmd ["-"] less
