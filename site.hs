{-# LANGUAGE DeriveDataTypeable, DoAndIfThenElse, OverloadedStrings #-}

import Control.Monad
import Data.Binary (Binary (..))
import Data.Typeable (Typeable (..))
import Hakyll
import System.Process (system)
import System.Exit (ExitCode (..))
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

    match "js/haxe/build.hxml" $ do
        route haxeRoute
        compile haxeCompiler

    match "pages/*" $ do
        route $ customRoute $ (`FP.replaceExtension` "html") . FP.takeFileName . toFilePath
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "games/*.md" $ do
        route $ setExtension "html"
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

haxeRoute :: Routes
haxeRoute = constRoute "js/dummy.js"

data HaxeFile = HaxeFile
    deriving (Show, Eq, Ord, Typeable)

instance Binary HaxeFile where
    put HaxeFile = return ()
    get          = return HaxeFile

instance Writable HaxeFile where
    write dst item = do
        let item'   = toFilePath $ itemIdentifier item
            hxml    = FP.takeFileName item'
            haxeDir = FP.takeDirectory item'
            jsBin   = haxeDir FP.</> "bin/*"
            dst'    = FP.takeDirectory dst
        ec <- system (cd haxeDir <&&> mkdir "bin" <&&> haxe hxml <&&> cd "-" <&&> cp jsBin dst')
        when (ec /= ExitSuccess) $ fail $ show ec

haxeCompiler :: Compiler (Item HaxeFile)
haxeCompiler = makeItem HaxeFile

(<&&>) :: String -> String -> String
(<&&>) = \a b -> a ++ " && " ++ b

cd :: FilePath -> String
cd = ("cd " ++)

mkdir :: FilePath -> String
mkdir = ("mkdir -p " ++)

haxe :: FilePath -> String
haxe = ("haxe " ++)

cp :: FilePath -> FilePath -> String
cp s d = "cp " ++ s ++ " " ++ d
