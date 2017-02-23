{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Monoid    ((<>))
import           Hakyll
import           System.Process

main :: IO ()
main = hakyll siteGenerator

siteGenerator :: Rules ()
siteGenerator = do
  -- match "static/css/*.css" compileCss
  match "static/**.less" compileLess
  match "templates/*" $ compile templateCompiler
  match "posts/*" compilePost

compilePost :: Rules ()
compilePost = do
  route $ setExtension "html"
  compile $ do
    item <- pandocCompiler
    item <- loadAndApplyTemplate "templates/post.html" defaultContext item
    loadAndApplyTemplate "templates/default.html" defaultContext item

compileCss :: Rules ()
compileCss =  do
  route idRoute
  compile copyFileCompiler

compileLess :: Rules ()
compileLess = do
  route $ setExtension "css" `composeRoutes` gsubRoute "less/" (const "css/")
  compile lessCompiler

lessCompiler :: Compiler (Item String)
lessCompiler = do
  less <- getResourceBody
  withItemBody callLessc less

  where
    callLessc less = unsafeCompiler $ do
      let cmd = "node_modules/less/bin/lessc"
          cmdArgs = ["-"]
          cmdInput = less
      readProcess cmd cmdArgs cmdInput
