module Language.GraphQL.Draft.Printer
  ( renderExecutableDoc,
  )
where

-------------------------------------------------------------------------------

import Data.Text (Text)
import {-# SOURCE #-} Language.GraphQL.Draft.Syntax (ExecutableDocument)
import Language.GraphQL.Draft.Syntax.Name (Name)

-------------------------------------------------------------------------------

renderExecutableDoc :: ExecutableDocument Name -> Text
