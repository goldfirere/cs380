-- Generate main course site
-- Copyright (c) Richard Eisenberg

{-# LANGUAGE OverloadedStrings, TypeApplications, ViewPatterns #-}

module Main where

import Hakyll
import Control.Monad
import Data.Monoid
import System.FilePath
import Text.Printf
import Data.List
import Debug.Trace

main :: IO ()
main = hakyll $ do
  match "web/css/*" $ do
    route   dropWebRoute
    compile compressCssCompiler

  match "web/images/*" $ do
    route   dropWebRoute
    compile copyFileCompiler

  match "labs/*" $ do
    route   $ idRoute
    compile $ copyFileCompiler

  match (fromRegex "^[0-9][0-9]_[^/]+/[^/]*\\.md") $ do
    route   $ dropClassNameRoute `composeRoutes` setHtmlExtension
    compile $ mdCompiler

  match (fromRegex "^[0-9][0-9]_[^/]+/images/.*") $ do
    route   $ dropClassNameRoute
    compile $ copyFileCompiler

  match (fromRegex "^[0-9][0-9]_[^/]+/[^0-9][^/]*\\.(pdf|txt|hs)" .&&. complement "**/quiz.pdf") $ do
    route   $ dropClassNameRoute
    compile $ copyFileCompiler

  match "hw/**/*.md" $ do
    route   $ homeworkRoute `composeRoutes` setExtension "html"
    compile $ mdCompiler

  match (complement "**/*.md" .&&. "hw/**") $ do
    route   $ homeworkRoute
    compile $ copyFileCompiler

   -- for the navbar
  match "web/*.md" $
    compile $ pandocCompiler

  match "**.md" $ do   -- catchall
    route   $ setExtension "html"
    compile $ mdCompiler

  match "web/templates/*.html" $ compile templateBodyCompiler

-- drop a "web/" prefix
dropWebRoute :: Routes
dropWebRoute = pathRoute tail

-- drop the XXX from 03_XXX/blah
dropClassNameRoute :: Routes
dropClassNameRoute = pathRoute $ \ (class_name : rest) -> take 2 class_name : rest

-- move from hw/NN/blah to hwXX/blah
homeworkRoute :: Routes
homeworkRoute = pathRoute $ \ (_hw : number : rest) -> ("hw" ++ number) : rest

setHtmlExtension :: Routes
setHtmlExtension = pathRoute $ \ (snocView -> (dirs, file)) ->
                                 dirs ++ [replaceExtension file "html"]

-- drop class name and "examples" from example file
exampleRoute :: Routes
exampleRoute = pathRoute $
    \ (class_name : _examples : example_name : _web_export : rest) ->
      take 2 class_name : example_name : rest

-- defaultContext + basename
wrapperContext :: Context String
wrapperContext =
  defaultContext <>
  field "basename" (return . takeBaseName . toFilePath . itemIdentifier)

pandocCompileThis :: Item String -> Compiler (Item String)
pandocCompileThis = (return . writePandoc) <=< readPandoc

-- do all the processing I expect on an md file
mdCompiler :: Compiler (Item String)
mdCompiler
  =   pandocCompiler
  >>= applyAsTemplate navbarContext
  >>= loadAndApplyTemplate "web/templates/wrapper.html" wrapperContext
  >>= relativizeUrls

navbarContext :: Context String
navbarContext = defaultContext <>
                field "navbar" (const $ loadBody (fromFilePath "web/navbar.md"))

------------------
-- a customRoute that alters the list of filepath components. Directories in
-- this list end with '/'
pathRoute :: ([FilePath] -> [FilePath]) -> Routes
pathRoute f = customRoute $ joinPath . f . splitPath . toFilePath

snocView :: [a] -> ([a], a)
snocView = go []
  where
    go acc [x]    = (reverse acc, x)
    go acc (x:xs) = go (x:acc) xs
    go _   _      = error "snocView on empty list"
