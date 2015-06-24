{-# LANGUAGE TupleSections, OverloadedStrings, ViewPatterns #-}
module Handler.Home where

import Import

import Data.Maybe
import qualified Data.Text.Lazy as LT
import qualified Filesystem as FS
import Shelly
import Text.Shakespeare.Text

defaultCode :: Text
defaultCode = [st|
message hoge {
  0: int moge
  1: map<string, double> hage
}

service test {
  void foo(0: hoge x)
}
|]

getHomeR :: Handler RepHtml
getHomeR = do
    let submission = Nothing :: Maybe (FileInfo, Text)
        handlerName = "getHomeR" :: Text
    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle "MessagePack IDL Code Generator"
        $(widgetFile "homepage")

postHomeR :: Handler (ContentType, Content)
postHomeR = do
  (fromMaybe "noname" -> name, source, lang, namespace) <- runInputPost $ (,,,)
    <$> iopt textField "name"
    <*> ireq textField "source"
    <*> ireq textField "lang"
    <*> iopt textField "namespace"

  let tarname = [lt|#{name}.tar.bz2|]
      idlname = [lt|#{name}.idl|]

  let opts = map LT.fromStrict $ case (lang, namespace) of
        ("cpp",  Just ns) -> ["-n", ns]
        ("java", Just pn) -> ["-p", pn]
        ("ruby", Just mn) -> ["-m", mn]
        _ -> []
  
  archive <- shelly $ do
    withTmpDir $ \tmppath -> chdir tmppath $ do
      writefile (fromText idlname) $ LT.fromStrict source
      run_ "mpidl" $ [LT.fromStrict lang, "-o", [lt|#{name}|], idlname] ++ opts
      run_ "tar" ["-cjf", tarname, [lt|#{name}|]]
      p <- pwd
      liftIO $ FS.readFile $ p </> fromText tarname

  setHeader "Content-Disposition" [st|attachment; filename="#{tarname}"|]
  return ("application/x-bz2", toContent archive)
