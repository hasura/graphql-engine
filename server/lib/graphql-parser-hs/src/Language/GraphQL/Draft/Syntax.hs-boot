module Language.GraphQL.Draft.Syntax
  ( ExecutableDocument,
    SchemaDocument,
  )
where

import Data.Kind (Type)

-------------------------------------------------------------------------------

type role ExecutableDocument nominal

type ExecutableDocument :: Type -> Type
data ExecutableDocument var

type SchemaDocument :: Type
data SchemaDocument
