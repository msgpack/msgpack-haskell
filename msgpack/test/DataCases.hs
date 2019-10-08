{-# LANGUAGE OverloadedStrings #-}

module DataCases (genDataCases) where

import           Control.Applicative        as App
import           Control.Monad
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as S
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Char
import qualified Data.Map                   as Map
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Word
import           Data.YAML                  as Y
import qualified GHC.Exts                   as Lst (fromList)
import           System.FilePath
import           Test.Tasty
import           Test.Tasty.HUnit

import           Data.MessagePack           hiding ((.:), (.=))
import           Data.MessagePack.Timestamp

genDataCases :: [FilePath] -> IO TestTree
genDataCases fns = testGroup "Reference Tests" <$> forM fns doFile
  where
    doFile fn = do
      let fn' = "test" </> "data" </> fn <.> "yaml"
      raw <- S.readFile fn'
      let Right [cases] = Y.decodeStrict raw

      tcs <- forM (zip [1..] cases) $ \(i,tc) -> do
        -- print (tc :: DataCase)
        App.pure $ testCase ("testcase #" ++ show (i::Int)) $ do
          -- test forward direction
          let b0 = L.toStrict $ pack obj
              obj = dcObject tc
          assertBool ("pack " ++ show obj)  (b0 `elem` dcMsgPack tc)

          forM_ (zip [0..] (dcMsgPack tc)) $ \(j,b) -> do
            let Right decoded = unpack (L.fromStrict b)

                packLbl   = "pack #" ++ (show (j::Int))
                unpackLbl = "un" ++ packLbl

            -- the `number` test-cases conflate integers and floats
            case (obj, decoded) of
              (ObjectDouble x, ObjectFloat _) -> do
                let obj' = ObjectFloat (realToFrac x)
                assertEqual packLbl b (L.toStrict $ pack obj')
                assertEqual unpackLbl  obj' decoded

              (ObjectInt x, ObjectFloat _) -> do
                let obj' = ObjectFloat (fromIntegral x)
                assertEqual packLbl b (L.toStrict $ pack obj')
                assertEqual unpackLbl  obj' decoded

              (ObjectInt x, ObjectDouble _) -> do
                let obj' = ObjectDouble (fromIntegral x)
                assertEqual packLbl b (L.toStrict $ pack obj')
                assertEqual unpackLbl  obj' decoded

              _ -> assertEqual unpackLbl obj decoded

          pure ()

      pure (testGroup fn tcs)


data DataCase = DataCase
  { dcMsgPack :: [BS.ByteString]
  , dcObject  :: Object
  } deriving Show

instance FromYAML DataCase where
  parseYAML = Y.withMap "DataCase" $ \m -> do
    msgpack <- m .: "msgpack"

    obj <- do { Just (Y.Scalar Y.SNull) <- m .:! "nil" ; pure ObjectNil }
       <|> do { Just b <- m .:! "bool" ; pure (ObjectBool b) }
       <|> do { Just i <- m .:! "number" ; pure (ObjectInt (fromInteger i)) }
       <|> do { Just s <- m .:! "bignum" ; pure (ObjectInt (read . T.unpack $ s)) }
       <|> do { Just d <- m .:! "number" ; pure (ObjectDouble d) }
       <|> do { Just t <- m .:! "string" ; pure (ObjectStr t) }
       <|> do { Just t <- m .:! "binary" ; pure (ObjectBin (hex2bin t)) }
       <|> do { Just v@(Y.Sequence _ _) <- m .:! "array"  ; pure (nodeToObj v) }
       <|> do { Just m'@(Y.Mapping _ _) <- m .:! "map"  ; pure (nodeToObj m') }
       <|> do { Just (n,t) <- m .:! "ext"  ; pure (ObjectExt n (hex2bin t)) }
       <|> do { Just (s,ns) <- m .:! "timestamp"; pure (toObject $ mptsFromPosixSeconds2 s ns) }

    pure (DataCase { dcMsgPack = map hex2bin msgpack, dcObject = obj })


nodeToObj :: Y.Node -> Object
nodeToObj (Y.Scalar sca)    = scalarToObj sca
nodeToObj (Y.Sequence _ ns) = ObjectArray (Lst.fromList (map nodeToObj ns))
nodeToObj (Y.Mapping _ ns)  = ObjectMap   (Lst.fromList $ map (\(k,v) -> (nodeToObj k, nodeToObj v)) $ Map.toList ns)
nodeToObj (Y.Anchor _ n)    = nodeToObj n

scalarToObj :: Y.Scalar -> Object
scalarToObj Y.SNull        = ObjectNil
scalarToObj (Y.SBool b)    = ObjectBool b
scalarToObj (Y.SFloat x)   = ObjectDouble x
scalarToObj (Y.SInt i)     = ObjectInt (fromInteger i)
scalarToObj (SStr t)       = ObjectStr t
scalarToObj (SUnknown _ _) = error "scalarToValue"

hex2bin :: Text -> S.ByteString
hex2bin t
  | T.null t  = BS.empty
  | otherwise = BS.pack (map f $ T.split (=='-') t)
  where
    f :: T.Text -> Word8
    f x | T.all isHexDigit x, [d1,d2] <- T.unpack x = read (['0','x',d1,d2])
        | otherwise = error ("hex2bin: " ++ show x)
