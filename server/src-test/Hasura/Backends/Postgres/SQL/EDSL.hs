-- | An EDSL for writing Postgres SQL expressions for testing purposes
--   as they are defined in "Hasura.Backends.Postgres.SQL.DML".
--
--   This is currently used in "Hasura.Backends.Postgres.SQL.Select.RenameIdentifiersSpec".
module Hasura.Backends.Postgres.SQL.EDSL
  ( module Hasura.Backends.Postgres.SQL.EDSL,
    module Hasura.Backends.Postgres.SQL.DML,
    module Hasura.Backends.Postgres.SQL.Types,
  )
where

import Data.List.NonEmpty qualified as NE (fromList)
import Hasura.Backends.Postgres.SQL.DML hiding (selectStar, selectStar')
import Hasura.Backends.Postgres.SQL.Types
import Hasura.Prelude hiding (exp)

-- * Extractors

-- | a common top level extractor
rootExtractor_ :: [Extractor]
rootExtractor_ =
  [ Extractor
      ( SEFnApp
          "coalesce"
          [ SEFnApp
              "json_agg"
              [SEIdentifier (Identifier {getIdenTxt = "root"})]
              Nothing,
            SELit "[]"
          ]
          Nothing
      )
      (Just (ColumnAlias {getColumnAlias = Identifier {getIdenTxt = "root"}}))
  ]

-- | a top level extractor with an order by clause
extractorOrd_ :: Text -> [OrderByItem] -> Maybe ColumnAlias -> [Extractor]
extractorOrd_ table orders alias =
  [ Extractor
      ( SEFnApp
          "coalesce"
          [ SEFnApp
              "json_agg"
              [ SEIdentifier
                  ( Identifier
                      { getIdenTxt = table
                      }
                  )
              ]
              ( Just
                  ( OrderByExp
                      (NE.fromList orders)
                  )
              ),
            SELit "[]"
          ]
          Nothing
      )
      alias
  ]

-- | Apply a @row_to_json@ function to expressions.
row_to_json_ :: [SQLExp] -> Maybe ColumnAlias -> Extractor
row_to_json_ exps = Extractor (SEFnApp "row_to_json" exps Nothing)

-- * Select

-- | Simple @select * from schema.table@ query.
selectStar_ :: Text -> Text -> Select
selectStar_ schema table =
  ( mkSelect
      { selExtr = [Extractor (SEStar Nothing) Nothing],
        selFrom =
          Just
            ( FromExp
                [ FISimple
                    ( QualifiedObject
                        { qSchema = SchemaName {getSchemaTxt = schema},
                          qName = TableName {getTableTxt = table}
                        }
                    )
                    Nothing
                ]
            )
      }
  )

-- | A query that selects specific columns from a table as an expression
selectIdentifiersFromExp_ :: Text -> Text -> [Text] -> Text -> SQLExp
selectIdentifiersFromExp_ idn table columns relAlias =
  SESelect
    ( mkSelect
        { selExtr = [Extractor (iden_ idn) Nothing],
          selFrom =
            from_
              [ mkSelect
                  { selExtr = map (\column -> tcolumn_ table column `asE_` column) columns
                  }
                  `as'_` relAlias
              ]
        }
    )

-- | Similar to the above but... different? I'm not sure why.
selectIdentifiers_ :: Text -> Text -> [Text] -> SQLExp
selectIdentifiers_ alias table columns =
  SESelect
    ( mkSelect
        { selExtr =
            [Extractor (SERowIdentifier (Identifier alias)) Nothing],
          selFrom =
            from_
              [ FISelect
                  (Lateral False)
                  ( mkSelect
                      { selExtr =
                          map
                            ( \column ->
                                Extractor
                                  ( SEQIdentifier
                                      ( QIdentifier
                                          (QualifiedIdentifier (TableIdentifier table) Nothing)
                                          (Identifier column)
                                      )
                                  )
                                  ( Just (ColumnAlias {getColumnAlias = Identifier column})
                                  )
                            )
                            columns
                      }
                  )
                  (TableAlias {getTableAlias = Identifier alias})
              ]
        }
    )

-- * Aliases

asT_ :: (Maybe TableAlias -> a) -> Text -> a
asT_ f alias = f (Just (TableAlias {getTableAlias = Identifier alias}))

asC_ :: (Maybe ColumnAlias -> a) -> Text -> a
asC_ f alias = f (Just (ColumnAlias {getColumnAlias = Identifier alias}))

as'_ :: Select -> Text -> FromItem
as'_ sel alias = FISelect (Lateral False) sel (TableAlias {getTableAlias = Identifier alias})

asE_ :: SQLExp -> Text -> Extractor
asE_ se alias = Extractor se (Just (ColumnAlias {getColumnAlias = Identifier alias}))

-- * From

from_ :: [FromItem] -> Maybe FromExp
from_ = Just . FromExp

-- * Columns

-- | Define a column expression as @schema.table.column@.
stcolumn_ :: Text -> Text -> Text -> SQLExp
stcolumn_ schema table column =
  SEQIdentifier
    ( QIdentifier
        ( QualTable
            ( QualifiedObject
                { qSchema = SchemaName schema,
                  qName = TableName table
                }
            )
        )
        (Identifier column)
    )

-- | Define a column expression as @table.column@.
tcolumn_ :: Text -> Text -> SQLExp
tcolumn_ table column =
  SEQIdentifier
    ( QIdentifier
        ( QualifiedIdentifier (TableIdentifier table) Nothing
        )
        ( Identifier
            { getIdenTxt = column
            }
        )
    )

-- | Define a column expression as @column@.
iden_ :: Text -> SQLExp
iden_ column = SEIdentifier $ Identifier {getIdenTxt = column}

-- | An arbitrary integer value.
int_ :: SQLExp
int_ = SETyAnn (SEPrep 2) (TypeAnn "integer")

-- * Where

-- | @WHERE@ clause.
where_ :: BoolExp -> Maybe WhereFrag
where_ = Just . WhereFrag

-- | Equality of two expressions.
eq_ :: SQLExp -> SQLExp -> BoolExp
eq_ = BECompare SEQ

-- * Joins

-- | Lateral left join. Used for relationships.
lateralLeftJoin_ :: FromItem -> FromItem -> [FromItem]
lateralLeftJoin_ left right =
  [ FIJoin
      ( JoinExpr
          left
          LeftOuter
          (withLateral right)
          (JoinOn (BELit True))
      )
  ]

withLateral :: FromItem -> FromItem
withLateral = \case
  FISelect _ sel al -> FISelect (Lateral True) sel al
  FISelectWith _ sel al -> FISelectWith (Lateral False) sel al
  x -> x

-- * Order by

-- | @ORDER BY@ clause.
orderby_ :: [OrderByItem] -> Maybe OrderByExp
orderby_ = Just . OrderByExp . NE.fromList

-- | an order by column set to ascending ordering.
asc_ :: Text -> OrderByItem
asc_ idn = OrderByItem (SEIdentifier (Identifier idn)) (Just OTAsc) (Just NullsLast)

-- * Limit

-- | @LIMIT 1@.
limit1_ :: Maybe LimitExp
limit1_ = Just (LimitExp (SEUnsafe "1"))
