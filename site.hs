{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid
import Hakyll
import qualified System.FilePath as FP


main :: IO ()
main = hakyllWith config $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*.scss" $ do
        route $ setExtension "css"
        compile $ getResourceString
            >>= withItemBody (unixFilter "sass" ["-s", "--scss", "-t", "compressed"])

    match "pages/*" $ do
        route $ customRoute $ (`FP.replaceExtension` "html") . FP.takeFileName . toFilePath
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ getResourceBody
            >>= loadAndApplyTemplate "templates/notitle.html" defaultContext
            >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


config :: Configuration
config = defaultConfiguration
    { deployCommand = "cp -r _site/* /home/uduki/fs/ssh/siphilia.net/home/siphilia/public_html/siphilia.net" }
