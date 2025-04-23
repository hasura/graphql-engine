module Hasura.Backends.MSSQL.Execute.QueryTags
  ( withQueryTags,
    withQueryTagsPrinter,
  )
where

import Database.ODBC.SQLServer qualified as ODBC
import Hasura.Backends.MSSQL.ToQuery as TQ
import Hasura.Prelude
import Hasura.QueryTags

withQueryTags :: ODBC.Query -> QueryTagsComment -> ODBC.Query
withQueryTags = addQueryTagsCommentGeneral (<>) ODBC.rawUnescapedText

withQueryTagsPrinter :: TQ.Printer -> QueryTagsComment -> TQ.Printer
withQueryTagsPrinter = addQueryTagsCommentGeneral (TQ.<+>) TQ.fromRawUnescapedText
