--
module Hasura.Server.Init.Arg.PrettyPrinter
  ( (PP.<$>),
    PP.Doc,
    PP.text,
    mkEnvVarDoc,
    mkExamplesDoc,
  )
where

--------------------------------------------------------------------------------

import Hasura.Prelude
import Text.PrettyPrint.ANSI.Leijen qualified as PP

--------------------------------------------------------------------------------

mkEnvVarDoc :: [(String, String)] -> PP.Doc
mkEnvVarDoc envVars =
  PP.text "Environment variables: "
    PP.<$> PP.indent 2 (PP.vsep $ map mkEnvVarLine envVars)
  where
    mkEnvVarLine (var, desc) =
      (PP.fillBreak 40 (PP.text var) PP.<+> prettifyDesc desc) <> PP.hardline
    prettifyDesc = PP.align . PP.fillSep . map PP.text . words

mkExamplesDoc :: [[String]] -> PP.Doc
mkExamplesDoc exampleLines =
  PP.text "Examples: " PP.<$> PP.indent 2 (PP.vsep examples)
  where
    examples = map PP.text $ intercalate [""] exampleLines
