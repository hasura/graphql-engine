module Language.GraphQL.Draft.Parser
  ( parseExecutableDoc,
    parseSchemaDocument,
  )
where

-------------------------------------------------------------------------------

import Data.Text (Text)
import {-# SOURCE #-} Language.GraphQL.Draft.Syntax qualified as AST
import Language.GraphQL.Draft.Syntax.Name (Name)
import Prelude (Either)

-------------------------------------------------------------------------------

parseExecutableDoc :: Text -> Either Text (AST.ExecutableDocument Name)
parseSchemaDocument :: Text -> Either Text AST.SchemaDocument
