{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Monoid    ((<>))
import           Hakyll
import           System.Process

main :: IO ()
main = hakyll siteGenerator

siteGenerator :: Rules ()
siteGenerator = do
  match "static/css/*.css" compileCss
  match "static/**.less" compileLess
  match "templates/**" $ compile templateCompiler
  match "posts/*.md" compilePost
  createIndex

createIndex :: Rules ()
createIndex = match "pages/index.html" $ do
  compile $ do
    posts <- recentFirst =<< loadAll "posts/*"
    let indexCtx = listField "posts" defaultContext (return posts)
                   <> defaultContext
    getResourceBody
      >>= applyAsTemplate indexCtx
      >>= loadAndApplyTemplate "templates/default.html" defaultContext

  route $ stripRoute "pages/"
    `composeRoutes` setExtension "html"

stripRoute :: String -> Routes
stripRoute = (`gsubRoute` const "")

compilePost :: Rules ()
compilePost = do
  compile $ pandocCompiler
    >>= loadAndApplyTemplate "templates/post.html" defaultContext
    >>= loadAndApplyTemplate "templates/default.html" defaultContext
  route $ setExtension "html"

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

