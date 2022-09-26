module Main
  ( main,
  )
where

-------------------------------------------------------------------------------

import Data.Bifunctor (second)
import Data.ByteString.Builder qualified as BS
import Data.Function ((&))
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text.Lazy.Builder qualified as LTB
import Data.Traversable (for)
import Language.GraphQL.Draft.Generator (genExecutableDocument, genText, generate)
import Language.GraphQL.Draft.Parser (parseExecutableDoc)
import Language.GraphQL.Draft.Printer (executableDocument, renderExecutableDoc)
import Language.GraphQL.Draft.Syntax (ExecutableDocument, Name, mkName)
import Prettyprinter qualified as PP
import Prettyprinter.Render.Text qualified as PP
import Test.Tasty.Bench (bench, bgroup, defaultMain, nf, whnf)
import Text.Builder qualified as STB -- Strict Text Builder
import Prelude

-------------------------------------------------------------------------------

genDocs :: Int -> IO [(Int, ExecutableDocument Name)]
genDocs num =
  for [1 .. num] $ \n -> (n,) <$> generate genExecutableDocument

genTexts :: Int -> IO [(Int, [Text])]
genTexts num =
  for [1 .. num] $ \n -> do
    texts <- for [1 .. 500 :: Int] \_ -> generate genText
    pure (n, texts)

main :: IO ()
main = do
  docs <- genDocs 10
  texts <- genTexts 10
  let grp1 = mkPPGrp docs
      grp2 = mkBBGrp docs
      grp3 = mkTBGrp docs
      grp4 = mkTLBGrp docs
      renderedDocs = map (second renderExecutableDoc) docs
      grp5 = mkPGrp renderedDocs
      grp6 = mkNGrp texts
  defaultMain [grp1, grp2, grp3, grp4, grp5, grp6]
  where
    mkNGrp texts =
      bgroup "checking name validity" $
        texts & map \(n, t) ->
          bench (show n) $ nf (length . mapMaybe mkName) t

    mkPGrp qs =
      bgroup "parsing executableDocument" $
        qs & map \(n, q) ->
          bench (show n) $ whnf parseExecutableDoc q

    mkPPGrp gqs =
      bgroup "rendering executableDocument (prettyprinter)" $
        gqs & map \(n, gq) ->
          bench (show n) $ nf (renderPP . executableDocument) gq

    mkBBGrp gqs =
      bgroup "rendering executableDocument (bytestring builder)" $
        gqs & map \(n, gq) ->
          bench (show n) $ nf (renderBB . executableDocument) gq

    mkTBGrp gqs =
      bgroup "rendering executableDocument (text builder)" $
        gqs & map \(n, gq) ->
          bench (show n) $ nf (renderTB . executableDocument) gq

    mkTLBGrp gqs =
      bgroup "rendering executableDocument (lazy text builder)" $
        gqs & map \(n, gq) ->
          bench (show n) $ nf (renderTLB . executableDocument) gq

    renderPP :: PP.Doc Text -> Text
    renderPP = PP.renderStrict . PP.layoutPretty PP.defaultLayoutOptions
    renderBB = BS.toLazyByteString
    renderTB = STB.run
    renderTLB = LTB.toLazyText
