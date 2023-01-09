{-# LANGUAGE ViewPatterns #-}

module Main
  ( main,
  )
where

-------------------------------------------------------------------------------

import BlockStrings (blockTest)
import Control.Monad (unless)
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding.Error qualified as TEE
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Builder qualified as LTB
import Data.Text.Lazy.Encoding qualified as LTE
import Hedgehog
  ( Group (..),
    Property,
    TestLimit,
    checkParallel,
    failure,
    footnote,
    forAll,
    property,
    withTests,
    (===),
  )
import Keywords qualified
import Language.GraphQL.Draft.Generator
import Language.GraphQL.Draft.Parser qualified as Input
import Language.GraphQL.Draft.Printer qualified as Output
import Language.GraphQL.Draft.Syntax
import Prettyprinter qualified as PP
import Prettyprinter.Render.Text qualified as PP
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Builder qualified as TB
import Prelude

-------------------------------------------------------------------------------

data TestMode = TMDev | TMQuick | TMRelease
  deriving stock (Show)

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    TMQuick -> runTest 100
    TMDev -> runTest 500
    TMRelease -> runTest 1000
  where
    parseArgs = foldr parseArg TMDev
    parseArg str _ = case str of
      "quick" -> TMQuick
      "release" -> TMRelease
      _ -> TMDev

runTest :: TestLimit -> IO ()
runTest limit = do
  allGood1 <- tests limit
  allGood2 <- blockTest
  unless (allGood1 && allGood2) exitFailure

tests :: TestLimit -> IO Bool
tests nTests =
  checkParallel $
    Group "Test.printer.parser" $
      [ ("property [ parse (prettyPrint ast) == ast ]", propParserPrettyPrinter nTests),
        ("property [ parse (textBuilderPrint ast) == ast ]", propParserTextPrinter nTests),
        ("property [ parse (lazyTextBuilderPrint ast) == ast ]", propParserLazyTextPrinter nTests),
        ("property [ parse (bytestringBuilderPrint ast) == ast ]", propParserBSPrinter nTests)
      ]
        ++ Keywords.primitiveTests

propParserPrettyPrinter :: TestLimit -> Property
propParserPrettyPrinter = mkPropParserPrinter $ prettyPrinter . Output.executableDocument
  where
    prettyPrinter :: PP.Doc Text -> Text
    prettyPrinter = PP.renderStrict . PP.layoutPretty PP.defaultLayoutOptions

propParserTextPrinter :: TestLimit -> Property
propParserTextPrinter = mkPropParserPrinter $ TB.run . Output.executableDocument

propParserLazyTextPrinter :: TestLimit -> Property
propParserLazyTextPrinter =
  mkPropParserPrinter $
    LT.toStrict
      . LTB.toLazyText
      . Output.executableDocument

propParserBSPrinter :: TestLimit -> Property
propParserBSPrinter =
  mkPropParserPrinter $
    bsToTxt
      . BSB.toLazyByteString
      . Output.executableDocument

mkPropParserPrinter :: (ExecutableDocument Name -> Text) -> (TestLimit -> Property)
mkPropParserPrinter printer = \space ->
  withTests space $
    property $ do
      xs <- forAll genExecutableDocument
      let rendered = printer xs
      either onError (xs ===) $ Input.parseExecutableDoc rendered
  where
    onError (T.unpack -> errorMsg) = do
      footnote errorMsg
      failure

bsToTxt :: LBS.ByteString -> Text
bsToTxt = LT.toStrict . LTE.decodeUtf8With TEE.lenientDecode
