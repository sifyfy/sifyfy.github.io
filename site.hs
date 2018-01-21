{-# LANGUAGE DeriveDataTypeable, DoAndIfThenElse, OverloadedStrings #-}

import           Control.Monad
import           Data.Binary     (Binary (..))
import           Data.Monoid     ((<>))
import           Data.Typeable   (Typeable (..))
import           Hakyll
import           System.Exit     (ExitCode (..))
import qualified System.FilePath as FP
import           System.Process  (system)

main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*.scss" $ do
        route $ setExtension "css"
        compile $ getResourceString
            >>= withItemBody (unixFilter "sass" ["-s", "--scss", "-t", "compressed"])

--    match "pages/*" $ do
--        route $ customRoute $ (`FP.replaceExtension` "html") . FP.takeFileName . toFilePath
--        compile $ pandocCompiler
--            >>= loadAndApplyTemplate "templates/default.html" defaultContext
--            >>= relativizeUrls

    match "notes/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["notes.html"] $ do
        route idRoute
        compile $ do
            posts <- loadPosts Nothing
            let archiveCtx =
                    listField "posts" postCtx (return posts) <>
                    constField "title" "Notes"               <>
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/notitle.html" archiveCtx
                >>= relativizeUrls

    create ["index.html"] $ do
        route idRoute
        compile $ do
            posts <- loadPosts $ Just 10
            let indexCtx =
                    listField "posts" postCtx (return posts) <>
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/index.html" indexCtx
                >>= loadAndApplyTemplate "templates/notitle.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

postCtx :: Context String
postCtx =
    dateField "date" "%Y/%m/%d" <>
    defaultContext

loadPosts :: (Binary a, Typeable a) => Maybe Int -> Compiler [Item a]
loadPosts limit = maybe id take limit <$> (recentFirst =<< loadAll "notes/*")
