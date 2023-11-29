module Hasura.Backends.MSSQL.Execute.QueryTags
  ( withQueryTags,
    withQueryTagsPrinter,
  )
where

import Database.ODBC.SQLServer qualified as ODBC
import Hasura.Backends.MSSQL.ToQuery as TQ
import Hasura.Prelude
import Hasura.QueryTags (QueryTagsComment (_unQueryTagsComment))

withQueryTags :: ODBC.Query -> QueryTagsComment -> ODBC.Query
withQueryTags query queryTags = query <> ODBC.rawUnescapedText (_unQueryTagsComment queryTags)

withQueryTagsPrinter :: TQ.Printer -> QueryTagsComment -> TQ.Printer
withQueryTagsPrinter printer queryTags = printer TQ.<+> TQ.fromRawUnescapedText (_unQueryTagsComment queryTags)
