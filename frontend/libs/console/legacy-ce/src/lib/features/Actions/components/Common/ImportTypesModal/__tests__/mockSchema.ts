export const schema = {
  __schema: {
    queryType: {
      name: 'query_root',
    },
    mutationType: {
      name: 'mutation_root',
    },
    subscriptionType: {
      name: 'subscription_root',
    },
    types: [
      {
        kind: 'SCALAR',
        name: 'Boolean',
        description: null,
        fields: null,
        inputFields: null,
        interfaces: null,
        enumValues: null,
        possibleTypes: null,
      },
      {
        kind: 'ENUM',
        name: 'CursorOrdering',
        description: 'ordering argument of a cursor',
        fields: null,
        inputFields: null,
        interfaces: null,
        enumValues: [
          {
            name: 'ASC',
            description: 'ascending ordering of the cursor',
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'DESC',
            description: 'descending ordering of the cursor',
            isDeprecated: false,
            deprecationReason: null,
          },
        ],
        possibleTypes: null,
      },
      {
        kind: 'SCALAR',
        name: 'Float',
        description: null,
        fields: null,
        inputFields: null,
        interfaces: null,
        enumValues: null,
        possibleTypes: null,
      },
      {
        kind: 'SCALAR',
        name: 'Int',
        description: null,
        fields: null,
        inputFields: null,
        interfaces: null,
        enumValues: null,
        possibleTypes: null,
      },
      {
        kind: 'INPUT_OBJECT',
        name: 'IntComparisonExp',
        description:
          'Boolean expression to compare columns of type "Int". All fields are combined with logical \'AND\'.',
        fields: null,
        inputFields: [
          {
            name: '_eq',
            description: null,
            type: {
              kind: 'SCALAR',
              name: 'Int',
              ofType: null,
            },
            defaultValue: null,
          },
          {
            name: '_gt',
            description: null,
            type: {
              kind: 'SCALAR',
              name: 'Int',
              ofType: null,
            },
            defaultValue: null,
          },
          {
            name: '_gte',
            description: null,
            type: {
              kind: 'SCALAR',
              name: 'Int',
              ofType: null,
            },
            defaultValue: null,
          },
          {
            name: '_in',
            description: null,
            type: {
              kind: 'LIST',
              name: null,
              ofType: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'SCALAR',
                  name: 'Int',
                  ofType: null,
                },
              },
            },
            defaultValue: null,
          },
          {
            name: '_isNull',
            description: null,
            type: {
              kind: 'SCALAR',
              name: 'Boolean',
              ofType: null,
            },
            defaultValue: null,
          },
          {
            name: '_lt',
            description: null,
            type: {
              kind: 'SCALAR',
              name: 'Int',
              ofType: null,
            },
            defaultValue: null,
          },
          {
            name: '_lte',
            description: null,
            type: {
              kind: 'SCALAR',
              name: 'Int',
              ofType: null,
            },
            defaultValue: null,
          },
          {
            name: '_neq',
            description: null,
            type: {
              kind: 'SCALAR',
              name: 'Int',
              ofType: null,
            },
            defaultValue: null,
          },
          {
            name: '_nin',
            description: null,
            type: {
              kind: 'LIST',
              name: null,
              ofType: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'SCALAR',
                  name: 'Int',
                  ofType: null,
                },
              },
            },
            defaultValue: null,
          },
        ],
        interfaces: null,
        enumValues: null,
        possibleTypes: null,
      },
      {
        kind: 'OBJECT',
        name: 'NewSchemaUserTable',
        description: 'columns and relationships of "new_schema.user_table"',
        fields: [
          {
            name: 'id',
            description: null,
            args: [],
            type: {
              kind: 'NON_NULL',
              name: null,
              ofType: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'name',
            description: null,
            args: [],
            type: {
              kind: 'NON_NULL',
              name: null,
              ofType: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
            },
            isDeprecated: false,
            deprecationReason: null,
          },
        ],
        inputFields: null,
        interfaces: [],
        enumValues: null,
        possibleTypes: null,
      },
      {
        kind: 'OBJECT',
        name: 'NewSchemaUserTableAggregate',
        description: 'aggregated selection of "new_schema.user_table"',
        fields: [
          {
            name: 'aggregate',
            description: null,
            args: [],
            type: {
              kind: 'OBJECT',
              name: 'NewSchemaUserTableAggregateFields',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'nodes',
            description: null,
            args: [],
            type: {
              kind: 'NON_NULL',
              name: null,
              ofType: {
                kind: 'LIST',
                name: null,
                ofType: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'OBJECT',
                    name: 'NewSchemaUserTable',
                    ofType: null,
                  },
                },
              },
            },
            isDeprecated: false,
            deprecationReason: null,
          },
        ],
        inputFields: null,
        interfaces: [],
        enumValues: null,
        possibleTypes: null,
      },
      {
        kind: 'OBJECT',
        name: 'NewSchemaUserTableAggregateFields',
        description: 'aggregate fields of "new_schema.user_table"',
        fields: [
          {
            name: 'avg',
            description: null,
            args: [],
            type: {
              kind: 'OBJECT',
              name: 'NewSchemaUserTableAvgFields',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'count',
            description: null,
            args: [
              {
                name: 'columns',
                description: null,
                type: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'ENUM',
                      name: 'NewSchemaUserTableSelectColumn',
                      ofType: null,
                    },
                  },
                },
                defaultValue: null,
              },
              {
                name: 'distinct',
                description: null,
                type: {
                  kind: 'SCALAR',
                  name: 'Boolean',
                  ofType: null,
                },
                defaultValue: null,
              },
            ],
            type: {
              kind: 'NON_NULL',
              name: null,
              ofType: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'max',
            description: null,
            args: [],
            type: {
              kind: 'OBJECT',
              name: 'NewSchemaUserTableMaxFields',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'min',
            description: null,
            args: [],
            type: {
              kind: 'OBJECT',
              name: 'NewSchemaUserTableMinFields',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'stddev',
            description: null,
            args: [],
            type: {
              kind: 'OBJECT',
              name: 'NewSchemaUserTableStddevFields',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'stddevPop',
            description: null,
            args: [],
            type: {
              kind: 'OBJECT',
              name: 'NewSchemaUserTableStddev_popFields',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'stddevSamp',
            description: null,
            args: [],
            type: {
              kind: 'OBJECT',
              name: 'NewSchemaUserTableStddev_sampFields',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'sum',
            description: null,
            args: [],
            type: {
              kind: 'OBJECT',
              name: 'NewSchemaUserTableSumFields',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'varPop',
            description: null,
            args: [],
            type: {
              kind: 'OBJECT',
              name: 'NewSchemaUserTableVar_popFields',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'varSamp',
            description: null,
            args: [],
            type: {
              kind: 'OBJECT',
              name: 'NewSchemaUserTableVar_sampFields',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'variance',
            description: null,
            args: [],
            type: {
              kind: 'OBJECT',
              name: 'NewSchemaUserTableVarianceFields',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
        ],
        inputFields: null,
        interfaces: [],
        enumValues: null,
        possibleTypes: null,
      },
      {
        kind: 'OBJECT',
        name: 'NewSchemaUserTableAvgFields',
        description: 'aggregate avg on columns',
        fields: [
          {
            name: 'id',
            description: null,
            args: [],
            type: {
              kind: 'SCALAR',
              name: 'Float',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
        ],
        inputFields: null,
        interfaces: [],
        enumValues: null,
        possibleTypes: null,
      },
      {
        kind: 'INPUT_OBJECT',
        name: 'NewSchemaUserTableBoolExp',
        description:
          'Boolean expression to filter rows from the table "new_schema.user_table". All fields are combined with a logical \'AND\'.',
        fields: null,
        inputFields: [
          {
            name: '_and',
            description: null,
            type: {
              kind: 'LIST',
              name: null,
              ofType: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'INPUT_OBJECT',
                  name: 'NewSchemaUserTableBoolExp',
                  ofType: null,
                },
              },
            },
            defaultValue: null,
          },
          {
            name: '_not',
            description: null,
            type: {
              kind: 'INPUT_OBJECT',
              name: 'NewSchemaUserTableBoolExp',
              ofType: null,
            },
            defaultValue: null,
          },
          {
            name: '_or',
            description: null,
            type: {
              kind: 'LIST',
              name: null,
              ofType: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'INPUT_OBJECT',
                  name: 'NewSchemaUserTableBoolExp',
                  ofType: null,
                },
              },
            },
            defaultValue: null,
          },
          {
            name: 'id',
            description: null,
            type: {
              kind: 'INPUT_OBJECT',
              name: 'IntComparisonExp',
              ofType: null,
            },
            defaultValue: null,
          },
          {
            name: 'name',
            description: null,
            type: {
              kind: 'INPUT_OBJECT',
              name: 'StringComparisonExp',
              ofType: null,
            },
            defaultValue: null,
          },
        ],
        interfaces: null,
        enumValues: null,
        possibleTypes: null,
      },
      {
        kind: 'ENUM',
        name: 'NewSchemaUserTableConstraint',
        description:
          'unique or primary key constraints on table "new_schema.user_table"',
        fields: null,
        inputFields: null,
        interfaces: null,
        enumValues: [
          {
            name: 'user_table_pkey',
            description: 'unique or primary key constraint on columns "id"',
            isDeprecated: false,
            deprecationReason: null,
          },
        ],
        possibleTypes: null,
      },
      {
        kind: 'INPUT_OBJECT',
        name: 'NewSchemaUserTableIncInput',
        description:
          'input type for incrementing numeric columns in table "new_schema.user_table"',
        fields: null,
        inputFields: [
          {
            name: 'id',
            description: null,
            type: {
              kind: 'SCALAR',
              name: 'Int',
              ofType: null,
            },
            defaultValue: null,
          },
        ],
        interfaces: null,
        enumValues: null,
        possibleTypes: null,
      },
      {
        kind: 'INPUT_OBJECT',
        name: 'NewSchemaUserTableInsertInput',
        description:
          'input type for inserting data into table "new_schema.user_table"',
        fields: null,
        inputFields: [
          {
            name: 'id',
            description: null,
            type: {
              kind: 'SCALAR',
              name: 'Int',
              ofType: null,
            },
            defaultValue: null,
          },
          {
            name: 'name',
            description: null,
            type: {
              kind: 'SCALAR',
              name: 'String',
              ofType: null,
            },
            defaultValue: null,
          },
        ],
        interfaces: null,
        enumValues: null,
        possibleTypes: null,
      },
      {
        kind: 'OBJECT',
        name: 'NewSchemaUserTableMaxFields',
        description: 'aggregate max on columns',
        fields: [
          {
            name: 'id',
            description: null,
            args: [],
            type: {
              kind: 'SCALAR',
              name: 'Int',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'name',
            description: null,
            args: [],
            type: {
              kind: 'SCALAR',
              name: 'String',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
        ],
        inputFields: null,
        interfaces: [],
        enumValues: null,
        possibleTypes: null,
      },
      {
        kind: 'OBJECT',
        name: 'NewSchemaUserTableMinFields',
        description: 'aggregate min on columns',
        fields: [
          {
            name: 'id',
            description: null,
            args: [],
            type: {
              kind: 'SCALAR',
              name: 'Int',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'name',
            description: null,
            args: [],
            type: {
              kind: 'SCALAR',
              name: 'String',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
        ],
        inputFields: null,
        interfaces: [],
        enumValues: null,
        possibleTypes: null,
      },
      {
        kind: 'OBJECT',
        name: 'NewSchemaUserTableMutationResponse',
        description:
          'response of any mutation on the table "new_schema.user_table"',
        fields: [
          {
            name: 'affected_rows',
            description: 'number of rows affected by the mutation',
            args: [],
            type: {
              kind: 'NON_NULL',
              name: null,
              ofType: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'returning',
            description: 'data from the rows affected by the mutation',
            args: [],
            type: {
              kind: 'NON_NULL',
              name: null,
              ofType: {
                kind: 'LIST',
                name: null,
                ofType: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'OBJECT',
                    name: 'NewSchemaUserTable',
                    ofType: null,
                  },
                },
              },
            },
            isDeprecated: false,
            deprecationReason: null,
          },
        ],
        inputFields: null,
        interfaces: [],
        enumValues: null,
        possibleTypes: null,
      },
      {
        kind: 'INPUT_OBJECT',
        name: 'NewSchemaUserTableOnConflict',
        description:
          'on_conflict condition type for table "new_schema.user_table"',
        fields: null,
        inputFields: [
          {
            name: 'constraint',
            description: null,
            type: {
              kind: 'NON_NULL',
              name: null,
              ofType: {
                kind: 'ENUM',
                name: 'NewSchemaUserTableConstraint',
                ofType: null,
              },
            },
            defaultValue: null,
          },
          {
            name: 'update_columns',
            description: null,
            type: {
              kind: 'NON_NULL',
              name: null,
              ofType: {
                kind: 'LIST',
                name: null,
                ofType: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'ENUM',
                    name: 'NewSchemaUserTableUpdateColumn',
                    ofType: null,
                  },
                },
              },
            },
            defaultValue: '[]',
          },
          {
            name: 'where',
            description: null,
            type: {
              kind: 'INPUT_OBJECT',
              name: 'NewSchemaUserTableBoolExp',
              ofType: null,
            },
            defaultValue: null,
          },
        ],
        interfaces: null,
        enumValues: null,
        possibleTypes: null,
      },
      {
        kind: 'INPUT_OBJECT',
        name: 'NewSchemaUserTableOrderBy',
        description:
          'Ordering options when selecting data from "new_schema.user_table".',
        fields: null,
        inputFields: [
          {
            name: 'id',
            description: null,
            type: {
              kind: 'ENUM',
              name: 'OrderBy',
              ofType: null,
            },
            defaultValue: null,
          },
          {
            name: 'name',
            description: null,
            type: {
              kind: 'ENUM',
              name: 'OrderBy',
              ofType: null,
            },
            defaultValue: null,
          },
        ],
        interfaces: null,
        enumValues: null,
        possibleTypes: null,
      },
      {
        kind: 'INPUT_OBJECT',
        name: 'NewSchemaUserTablePkColumnsInput',
        description:
          'primary key columns input for table: new_schema.user_table',
        fields: null,
        inputFields: [
          {
            name: 'id',
            description: null,
            type: {
              kind: 'NON_NULL',
              name: null,
              ofType: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
            },
            defaultValue: null,
          },
        ],
        interfaces: null,
        enumValues: null,
        possibleTypes: null,
      },
      {
        kind: 'ENUM',
        name: 'NewSchemaUserTableSelectColumn',
        description: 'select columns of table "new_schema.user_table"',
        fields: null,
        inputFields: null,
        interfaces: null,
        enumValues: [
          {
            name: 'id',
            description: 'column name',
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'name',
            description: 'column name',
            isDeprecated: false,
            deprecationReason: null,
          },
        ],
        possibleTypes: null,
      },
      {
        kind: 'INPUT_OBJECT',
        name: 'NewSchemaUserTableSetInput',
        description:
          'input type for updating data in table "new_schema.user_table"',
        fields: null,
        inputFields: [
          {
            name: 'id',
            description: null,
            type: {
              kind: 'SCALAR',
              name: 'Int',
              ofType: null,
            },
            defaultValue: null,
          },
          {
            name: 'name',
            description: null,
            type: {
              kind: 'SCALAR',
              name: 'String',
              ofType: null,
            },
            defaultValue: null,
          },
        ],
        interfaces: null,
        enumValues: null,
        possibleTypes: null,
      },
      {
        kind: 'OBJECT',
        name: 'NewSchemaUserTableStddevFields',
        description: 'aggregate stddev on columns',
        fields: [
          {
            name: 'id',
            description: null,
            args: [],
            type: {
              kind: 'SCALAR',
              name: 'Float',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
        ],
        inputFields: null,
        interfaces: [],
        enumValues: null,
        possibleTypes: null,
      },
      {
        kind: 'OBJECT',
        name: 'NewSchemaUserTableStddev_popFields',
        description: 'aggregate stddev_pop on columns',
        fields: [
          {
            name: 'id',
            description: null,
            args: [],
            type: {
              kind: 'SCALAR',
              name: 'Float',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
        ],
        inputFields: null,
        interfaces: [],
        enumValues: null,
        possibleTypes: null,
      },
      {
        kind: 'OBJECT',
        name: 'NewSchemaUserTableStddev_sampFields',
        description: 'aggregate stddev_samp on columns',
        fields: [
          {
            name: 'id',
            description: null,
            args: [],
            type: {
              kind: 'SCALAR',
              name: 'Float',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
        ],
        inputFields: null,
        interfaces: [],
        enumValues: null,
        possibleTypes: null,
      },
      {
        kind: 'OBJECT',
        name: 'NewSchemaUserTableSumFields',
        description: 'aggregate sum on columns',
        fields: [
          {
            name: 'id',
            description: null,
            args: [],
            type: {
              kind: 'SCALAR',
              name: 'Int',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
        ],
        inputFields: null,
        interfaces: [],
        enumValues: null,
        possibleTypes: null,
      },
      {
        kind: 'ENUM',
        name: 'NewSchemaUserTableUpdateColumn',
        description: 'update columns of table "new_schema.user_table"',
        fields: null,
        inputFields: null,
        interfaces: null,
        enumValues: [
          {
            name: 'id',
            description: 'column name',
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'name',
            description: 'column name',
            isDeprecated: false,
            deprecationReason: null,
          },
        ],
        possibleTypes: null,
      },
      {
        kind: 'INPUT_OBJECT',
        name: 'NewSchemaUserTableUpdates',
        description: null,
        fields: null,
        inputFields: [
          {
            name: '_inc',
            description:
              'increments the numeric columns with given value of the filtered values',
            type: {
              kind: 'INPUT_OBJECT',
              name: 'NewSchemaUserTableIncInput',
              ofType: null,
            },
            defaultValue: null,
          },
          {
            name: '_set',
            description:
              'sets the columns of the filtered rows to the given values',
            type: {
              kind: 'INPUT_OBJECT',
              name: 'NewSchemaUserTableSetInput',
              ofType: null,
            },
            defaultValue: null,
          },
          {
            name: 'where',
            description: null,
            type: {
              kind: 'NON_NULL',
              name: null,
              ofType: {
                kind: 'INPUT_OBJECT',
                name: 'NewSchemaUserTableBoolExp',
                ofType: null,
              },
            },
            defaultValue: null,
          },
        ],
        interfaces: null,
        enumValues: null,
        possibleTypes: null,
      },
      {
        kind: 'OBJECT',
        name: 'NewSchemaUserTableVar_popFields',
        description: 'aggregate var_pop on columns',
        fields: [
          {
            name: 'id',
            description: null,
            args: [],
            type: {
              kind: 'SCALAR',
              name: 'Float',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
        ],
        inputFields: null,
        interfaces: [],
        enumValues: null,
        possibleTypes: null,
      },
      {
        kind: 'OBJECT',
        name: 'NewSchemaUserTableVar_sampFields',
        description: 'aggregate var_samp on columns',
        fields: [
          {
            name: 'id',
            description: null,
            args: [],
            type: {
              kind: 'SCALAR',
              name: 'Float',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
        ],
        inputFields: null,
        interfaces: [],
        enumValues: null,
        possibleTypes: null,
      },
      {
        kind: 'OBJECT',
        name: 'NewSchemaUserTableVarianceFields',
        description: 'aggregate variance on columns',
        fields: [
          {
            name: 'id',
            description: null,
            args: [],
            type: {
              kind: 'SCALAR',
              name: 'Float',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
        ],
        inputFields: null,
        interfaces: [],
        enumValues: null,
        possibleTypes: null,
      },
      {
        kind: 'ENUM',
        name: 'OrderBy',
        description: 'column ordering options',
        fields: null,
        inputFields: null,
        interfaces: null,
        enumValues: [
          {
            name: 'ASC',
            description: 'in ascending order, nulls last',
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'ASC_NULLS_FIRST',
            description: 'in ascending order, nulls first',
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'ASC_NULLS_LAST',
            description: 'in ascending order, nulls last',
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'DESC',
            description: 'in descending order, nulls first',
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'DESC_NULLS_FIRST',
            description: 'in descending order, nulls first',
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'DESC_NULLS_LAST',
            description: 'in descending order, nulls last',
            isDeprecated: false,
            deprecationReason: null,
          },
        ],
        possibleTypes: null,
      },
      {
        kind: 'SCALAR',
        name: 'String',
        description: null,
        fields: null,
        inputFields: null,
        interfaces: null,
        enumValues: null,
        possibleTypes: null,
      },
      {
        kind: 'INPUT_OBJECT',
        name: 'StringComparisonExp',
        description:
          'Boolean expression to compare columns of type "String". All fields are combined with logical \'AND\'.',
        fields: null,
        inputFields: [
          {
            name: '_eq',
            description: null,
            type: {
              kind: 'SCALAR',
              name: 'String',
              ofType: null,
            },
            defaultValue: null,
          },
          {
            name: '_gt',
            description: null,
            type: {
              kind: 'SCALAR',
              name: 'String',
              ofType: null,
            },
            defaultValue: null,
          },
          {
            name: '_gte',
            description: null,
            type: {
              kind: 'SCALAR',
              name: 'String',
              ofType: null,
            },
            defaultValue: null,
          },
          {
            name: '_ilike',
            description:
              'does the column match the given case-insensitive pattern',
            type: {
              kind: 'SCALAR',
              name: 'String',
              ofType: null,
            },
            defaultValue: null,
          },
          {
            name: '_in',
            description: null,
            type: {
              kind: 'LIST',
              name: null,
              ofType: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'SCALAR',
                  name: 'String',
                  ofType: null,
                },
              },
            },
            defaultValue: null,
          },
          {
            name: '_iregex',
            description:
              'does the column match the given POSIX regular expression, case insensitive',
            type: {
              kind: 'SCALAR',
              name: 'String',
              ofType: null,
            },
            defaultValue: null,
          },
          {
            name: '_isNull',
            description: null,
            type: {
              kind: 'SCALAR',
              name: 'Boolean',
              ofType: null,
            },
            defaultValue: null,
          },
          {
            name: '_like',
            description: 'does the column match the given pattern',
            type: {
              kind: 'SCALAR',
              name: 'String',
              ofType: null,
            },
            defaultValue: null,
          },
          {
            name: '_lt',
            description: null,
            type: {
              kind: 'SCALAR',
              name: 'String',
              ofType: null,
            },
            defaultValue: null,
          },
          {
            name: '_lte',
            description: null,
            type: {
              kind: 'SCALAR',
              name: 'String',
              ofType: null,
            },
            defaultValue: null,
          },
          {
            name: '_neq',
            description: null,
            type: {
              kind: 'SCALAR',
              name: 'String',
              ofType: null,
            },
            defaultValue: null,
          },
          {
            name: '_nilike',
            description:
              'does the column NOT match the given case-insensitive pattern',
            type: {
              kind: 'SCALAR',
              name: 'String',
              ofType: null,
            },
            defaultValue: null,
          },
          {
            name: '_nin',
            description: null,
            type: {
              kind: 'LIST',
              name: null,
              ofType: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'SCALAR',
                  name: 'String',
                  ofType: null,
                },
              },
            },
            defaultValue: null,
          },
          {
            name: '_niregex',
            description:
              'does the column NOT match the given POSIX regular expression, case insensitive',
            type: {
              kind: 'SCALAR',
              name: 'String',
              ofType: null,
            },
            defaultValue: null,
          },
          {
            name: '_nlike',
            description: 'does the column NOT match the given pattern',
            type: {
              kind: 'SCALAR',
              name: 'String',
              ofType: null,
            },
            defaultValue: null,
          },
          {
            name: '_nregex',
            description:
              'does the column NOT match the given POSIX regular expression, case sensitive',
            type: {
              kind: 'SCALAR',
              name: 'String',
              ofType: null,
            },
            defaultValue: null,
          },
          {
            name: '_nsimilar',
            description:
              'does the column NOT match the given SQL regular expression',
            type: {
              kind: 'SCALAR',
              name: 'String',
              ofType: null,
            },
            defaultValue: null,
          },
          {
            name: '_regex',
            description:
              'does the column match the given POSIX regular expression, case sensitive',
            type: {
              kind: 'SCALAR',
              name: 'String',
              ofType: null,
            },
            defaultValue: null,
          },
          {
            name: '_similar',
            description:
              'does the column match the given SQL regular expression',
            type: {
              kind: 'SCALAR',
              name: 'String',
              ofType: null,
            },
            defaultValue: null,
          },
        ],
        interfaces: null,
        enumValues: null,
        possibleTypes: null,
      },
      {
        kind: 'OBJECT',
        name: 'Table',
        description: 'columns and relationships of "table"',
        fields: [
          {
            name: 'id',
            description: null,
            args: [],
            type: {
              kind: 'NON_NULL',
              name: null,
              ofType: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'name',
            description: null,
            args: [],
            type: {
              kind: 'NON_NULL',
              name: null,
              ofType: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
            },
            isDeprecated: false,
            deprecationReason: null,
          },
        ],
        inputFields: null,
        interfaces: [],
        enumValues: null,
        possibleTypes: null,
      },
      {
        kind: 'OBJECT',
        name: 'TableAggregate',
        description: 'aggregated selection of "table"',
        fields: [
          {
            name: 'aggregate',
            description: null,
            args: [],
            type: {
              kind: 'OBJECT',
              name: 'TableAggregateFields',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'nodes',
            description: null,
            args: [],
            type: {
              kind: 'NON_NULL',
              name: null,
              ofType: {
                kind: 'LIST',
                name: null,
                ofType: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'OBJECT',
                    name: 'Table',
                    ofType: null,
                  },
                },
              },
            },
            isDeprecated: false,
            deprecationReason: null,
          },
        ],
        inputFields: null,
        interfaces: [],
        enumValues: null,
        possibleTypes: null,
      },
      {
        kind: 'OBJECT',
        name: 'TableAggregateFields',
        description: 'aggregate fields of "table"',
        fields: [
          {
            name: 'avg',
            description: null,
            args: [],
            type: {
              kind: 'OBJECT',
              name: 'TableAvgFields',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'count',
            description: null,
            args: [
              {
                name: 'columns',
                description: null,
                type: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'ENUM',
                      name: 'TableSelectColumn',
                      ofType: null,
                    },
                  },
                },
                defaultValue: null,
              },
              {
                name: 'distinct',
                description: null,
                type: {
                  kind: 'SCALAR',
                  name: 'Boolean',
                  ofType: null,
                },
                defaultValue: null,
              },
            ],
            type: {
              kind: 'NON_NULL',
              name: null,
              ofType: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'max',
            description: null,
            args: [],
            type: {
              kind: 'OBJECT',
              name: 'TableMaxFields',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'min',
            description: null,
            args: [],
            type: {
              kind: 'OBJECT',
              name: 'TableMinFields',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'stddev',
            description: null,
            args: [],
            type: {
              kind: 'OBJECT',
              name: 'TableStddevFields',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'stddevPop',
            description: null,
            args: [],
            type: {
              kind: 'OBJECT',
              name: 'TableStddev_popFields',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'stddevSamp',
            description: null,
            args: [],
            type: {
              kind: 'OBJECT',
              name: 'TableStddev_sampFields',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'sum',
            description: null,
            args: [],
            type: {
              kind: 'OBJECT',
              name: 'TableSumFields',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'varPop',
            description: null,
            args: [],
            type: {
              kind: 'OBJECT',
              name: 'TableVar_popFields',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'varSamp',
            description: null,
            args: [],
            type: {
              kind: 'OBJECT',
              name: 'TableVar_sampFields',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'variance',
            description: null,
            args: [],
            type: {
              kind: 'OBJECT',
              name: 'TableVarianceFields',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
        ],
        inputFields: null,
        interfaces: [],
        enumValues: null,
        possibleTypes: null,
      },
      {
        kind: 'OBJECT',
        name: 'TableAvgFields',
        description: 'aggregate avg on columns',
        fields: [
          {
            name: 'id',
            description: null,
            args: [],
            type: {
              kind: 'SCALAR',
              name: 'Float',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
        ],
        inputFields: null,
        interfaces: [],
        enumValues: null,
        possibleTypes: null,
      },
      {
        kind: 'INPUT_OBJECT',
        name: 'TableBoolExp',
        description:
          'Boolean expression to filter rows from the table "table". All fields are combined with a logical \'AND\'.',
        fields: null,
        inputFields: [
          {
            name: '_and',
            description: null,
            type: {
              kind: 'LIST',
              name: null,
              ofType: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'INPUT_OBJECT',
                  name: 'TableBoolExp',
                  ofType: null,
                },
              },
            },
            defaultValue: null,
          },
          {
            name: '_not',
            description: null,
            type: {
              kind: 'INPUT_OBJECT',
              name: 'TableBoolExp',
              ofType: null,
            },
            defaultValue: null,
          },
          {
            name: '_or',
            description: null,
            type: {
              kind: 'LIST',
              name: null,
              ofType: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'INPUT_OBJECT',
                  name: 'TableBoolExp',
                  ofType: null,
                },
              },
            },
            defaultValue: null,
          },
          {
            name: 'id',
            description: null,
            type: {
              kind: 'INPUT_OBJECT',
              name: 'IntComparisonExp',
              ofType: null,
            },
            defaultValue: null,
          },
          {
            name: 'name',
            description: null,
            type: {
              kind: 'INPUT_OBJECT',
              name: 'StringComparisonExp',
              ofType: null,
            },
            defaultValue: null,
          },
        ],
        interfaces: null,
        enumValues: null,
        possibleTypes: null,
      },
      {
        kind: 'ENUM',
        name: 'TableConstraint',
        description: 'unique or primary key constraints on table "table"',
        fields: null,
        inputFields: null,
        interfaces: null,
        enumValues: [
          {
            name: 'table_pkey',
            description: 'unique or primary key constraint on columns "id"',
            isDeprecated: false,
            deprecationReason: null,
          },
        ],
        possibleTypes: null,
      },
      {
        kind: 'INPUT_OBJECT',
        name: 'TableIncInput',
        description:
          'input type for incrementing numeric columns in table "table"',
        fields: null,
        inputFields: [
          {
            name: 'id',
            description: null,
            type: {
              kind: 'SCALAR',
              name: 'Int',
              ofType: null,
            },
            defaultValue: null,
          },
        ],
        interfaces: null,
        enumValues: null,
        possibleTypes: null,
      },
      {
        kind: 'INPUT_OBJECT',
        name: 'TableInsertInput',
        description: 'input type for inserting data into table "table"',
        fields: null,
        inputFields: [
          {
            name: 'id',
            description: null,
            type: {
              kind: 'SCALAR',
              name: 'Int',
              ofType: null,
            },
            defaultValue: null,
          },
          {
            name: 'name',
            description: null,
            type: {
              kind: 'SCALAR',
              name: 'String',
              ofType: null,
            },
            defaultValue: null,
          },
        ],
        interfaces: null,
        enumValues: null,
        possibleTypes: null,
      },
      {
        kind: 'OBJECT',
        name: 'TableMaxFields',
        description: 'aggregate max on columns',
        fields: [
          {
            name: 'id',
            description: null,
            args: [],
            type: {
              kind: 'SCALAR',
              name: 'Int',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'name',
            description: null,
            args: [],
            type: {
              kind: 'SCALAR',
              name: 'String',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
        ],
        inputFields: null,
        interfaces: [],
        enumValues: null,
        possibleTypes: null,
      },
      {
        kind: 'OBJECT',
        name: 'TableMinFields',
        description: 'aggregate min on columns',
        fields: [
          {
            name: 'id',
            description: null,
            args: [],
            type: {
              kind: 'SCALAR',
              name: 'Int',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'name',
            description: null,
            args: [],
            type: {
              kind: 'SCALAR',
              name: 'String',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
        ],
        inputFields: null,
        interfaces: [],
        enumValues: null,
        possibleTypes: null,
      },
      {
        kind: 'OBJECT',
        name: 'TableMutationResponse',
        description: 'response of any mutation on the table "table"',
        fields: [
          {
            name: 'affected_rows',
            description: 'number of rows affected by the mutation',
            args: [],
            type: {
              kind: 'NON_NULL',
              name: null,
              ofType: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'returning',
            description: 'data from the rows affected by the mutation',
            args: [],
            type: {
              kind: 'NON_NULL',
              name: null,
              ofType: {
                kind: 'LIST',
                name: null,
                ofType: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'OBJECT',
                    name: 'Table',
                    ofType: null,
                  },
                },
              },
            },
            isDeprecated: false,
            deprecationReason: null,
          },
        ],
        inputFields: null,
        interfaces: [],
        enumValues: null,
        possibleTypes: null,
      },
      {
        kind: 'INPUT_OBJECT',
        name: 'TableOnConflict',
        description: 'on_conflict condition type for table "table"',
        fields: null,
        inputFields: [
          {
            name: 'constraint',
            description: null,
            type: {
              kind: 'NON_NULL',
              name: null,
              ofType: {
                kind: 'ENUM',
                name: 'TableConstraint',
                ofType: null,
              },
            },
            defaultValue: null,
          },
          {
            name: 'update_columns',
            description: null,
            type: {
              kind: 'NON_NULL',
              name: null,
              ofType: {
                kind: 'LIST',
                name: null,
                ofType: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'ENUM',
                    name: 'TableUpdateColumn',
                    ofType: null,
                  },
                },
              },
            },
            defaultValue: '[]',
          },
          {
            name: 'where',
            description: null,
            type: {
              kind: 'INPUT_OBJECT',
              name: 'TableBoolExp',
              ofType: null,
            },
            defaultValue: null,
          },
        ],
        interfaces: null,
        enumValues: null,
        possibleTypes: null,
      },
      {
        kind: 'INPUT_OBJECT',
        name: 'TableOrderBy',
        description: 'Ordering options when selecting data from "table".',
        fields: null,
        inputFields: [
          {
            name: 'id',
            description: null,
            type: {
              kind: 'ENUM',
              name: 'OrderBy',
              ofType: null,
            },
            defaultValue: null,
          },
          {
            name: 'name',
            description: null,
            type: {
              kind: 'ENUM',
              name: 'OrderBy',
              ofType: null,
            },
            defaultValue: null,
          },
        ],
        interfaces: null,
        enumValues: null,
        possibleTypes: null,
      },
      {
        kind: 'INPUT_OBJECT',
        name: 'TablePkColumnsInput',
        description: 'primary key columns input for table: table',
        fields: null,
        inputFields: [
          {
            name: 'id',
            description: null,
            type: {
              kind: 'NON_NULL',
              name: null,
              ofType: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
            },
            defaultValue: null,
          },
        ],
        interfaces: null,
        enumValues: null,
        possibleTypes: null,
      },
      {
        kind: 'ENUM',
        name: 'TableSelectColumn',
        description: 'select columns of table "table"',
        fields: null,
        inputFields: null,
        interfaces: null,
        enumValues: [
          {
            name: 'id',
            description: 'column name',
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'name',
            description: 'column name',
            isDeprecated: false,
            deprecationReason: null,
          },
        ],
        possibleTypes: null,
      },
      {
        kind: 'INPUT_OBJECT',
        name: 'TableSetInput',
        description: 'input type for updating data in table "table"',
        fields: null,
        inputFields: [
          {
            name: 'id',
            description: null,
            type: {
              kind: 'SCALAR',
              name: 'Int',
              ofType: null,
            },
            defaultValue: null,
          },
          {
            name: 'name',
            description: null,
            type: {
              kind: 'SCALAR',
              name: 'String',
              ofType: null,
            },
            defaultValue: null,
          },
        ],
        interfaces: null,
        enumValues: null,
        possibleTypes: null,
      },
      {
        kind: 'OBJECT',
        name: 'TableStddevFields',
        description: 'aggregate stddev on columns',
        fields: [
          {
            name: 'id',
            description: null,
            args: [],
            type: {
              kind: 'SCALAR',
              name: 'Float',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
        ],
        inputFields: null,
        interfaces: [],
        enumValues: null,
        possibleTypes: null,
      },
      {
        kind: 'OBJECT',
        name: 'TableStddev_popFields',
        description: 'aggregate stddev_pop on columns',
        fields: [
          {
            name: 'id',
            description: null,
            args: [],
            type: {
              kind: 'SCALAR',
              name: 'Float',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
        ],
        inputFields: null,
        interfaces: [],
        enumValues: null,
        possibleTypes: null,
      },
      {
        kind: 'OBJECT',
        name: 'TableStddev_sampFields',
        description: 'aggregate stddev_samp on columns',
        fields: [
          {
            name: 'id',
            description: null,
            args: [],
            type: {
              kind: 'SCALAR',
              name: 'Float',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
        ],
        inputFields: null,
        interfaces: [],
        enumValues: null,
        possibleTypes: null,
      },
      {
        kind: 'OBJECT',
        name: 'TableSumFields',
        description: 'aggregate sum on columns',
        fields: [
          {
            name: 'id',
            description: null,
            args: [],
            type: {
              kind: 'SCALAR',
              name: 'Int',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
        ],
        inputFields: null,
        interfaces: [],
        enumValues: null,
        possibleTypes: null,
      },
      {
        kind: 'ENUM',
        name: 'TableUpdateColumn',
        description: 'update columns of table "table"',
        fields: null,
        inputFields: null,
        interfaces: null,
        enumValues: [
          {
            name: 'id',
            description: 'column name',
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'name',
            description: 'column name',
            isDeprecated: false,
            deprecationReason: null,
          },
        ],
        possibleTypes: null,
      },
      {
        kind: 'INPUT_OBJECT',
        name: 'TableUpdates',
        description: null,
        fields: null,
        inputFields: [
          {
            name: '_inc',
            description:
              'increments the numeric columns with given value of the filtered values',
            type: {
              kind: 'INPUT_OBJECT',
              name: 'TableIncInput',
              ofType: null,
            },
            defaultValue: null,
          },
          {
            name: '_set',
            description:
              'sets the columns of the filtered rows to the given values',
            type: {
              kind: 'INPUT_OBJECT',
              name: 'TableSetInput',
              ofType: null,
            },
            defaultValue: null,
          },
          {
            name: 'where',
            description: null,
            type: {
              kind: 'NON_NULL',
              name: null,
              ofType: {
                kind: 'INPUT_OBJECT',
                name: 'TableBoolExp',
                ofType: null,
              },
            },
            defaultValue: null,
          },
        ],
        interfaces: null,
        enumValues: null,
        possibleTypes: null,
      },
      {
        kind: 'OBJECT',
        name: 'TableVar_popFields',
        description: 'aggregate var_pop on columns',
        fields: [
          {
            name: 'id',
            description: null,
            args: [],
            type: {
              kind: 'SCALAR',
              name: 'Float',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
        ],
        inputFields: null,
        interfaces: [],
        enumValues: null,
        possibleTypes: null,
      },
      {
        kind: 'OBJECT',
        name: 'TableVar_sampFields',
        description: 'aggregate var_samp on columns',
        fields: [
          {
            name: 'id',
            description: null,
            args: [],
            type: {
              kind: 'SCALAR',
              name: 'Float',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
        ],
        inputFields: null,
        interfaces: [],
        enumValues: null,
        possibleTypes: null,
      },
      {
        kind: 'OBJECT',
        name: 'TableVarianceFields',
        description: 'aggregate variance on columns',
        fields: [
          {
            name: 'id',
            description: null,
            args: [],
            type: {
              kind: 'SCALAR',
              name: 'Float',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
        ],
        inputFields: null,
        interfaces: [],
        enumValues: null,
        possibleTypes: null,
      },
      {
        kind: 'OBJECT',
        name: '__Directive',
        description: null,
        fields: [
          {
            name: 'args',
            description: null,
            args: [],
            type: {
              kind: 'OBJECT',
              name: '__InputValue',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'description',
            description: null,
            args: [],
            type: {
              kind: 'NON_NULL',
              name: null,
              ofType: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'isRepeatable',
            description: null,
            args: [],
            type: {
              kind: 'NON_NULL',
              name: null,
              ofType: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'locations',
            description: null,
            args: [],
            type: {
              kind: 'NON_NULL',
              name: null,
              ofType: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'name',
            description: null,
            args: [],
            type: {
              kind: 'NON_NULL',
              name: null,
              ofType: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
            },
            isDeprecated: false,
            deprecationReason: null,
          },
        ],
        inputFields: null,
        interfaces: [],
        enumValues: null,
        possibleTypes: null,
      },
      {
        kind: 'OBJECT',
        name: '__EnumValue',
        description: null,
        fields: [
          {
            name: 'deprecationReason',
            description: null,
            args: [],
            type: {
              kind: 'NON_NULL',
              name: null,
              ofType: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'description',
            description: null,
            args: [],
            type: {
              kind: 'NON_NULL',
              name: null,
              ofType: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'isDeprecated',
            description: null,
            args: [],
            type: {
              kind: 'NON_NULL',
              name: null,
              ofType: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'name',
            description: null,
            args: [],
            type: {
              kind: 'NON_NULL',
              name: null,
              ofType: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
            },
            isDeprecated: false,
            deprecationReason: null,
          },
        ],
        inputFields: null,
        interfaces: [],
        enumValues: null,
        possibleTypes: null,
      },
      {
        kind: 'OBJECT',
        name: '__Field',
        description: null,
        fields: [
          {
            name: 'args',
            description: null,
            args: [],
            type: {
              kind: 'OBJECT',
              name: '__InputValue',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'deprecationReason',
            description: null,
            args: [],
            type: {
              kind: 'NON_NULL',
              name: null,
              ofType: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'description',
            description: null,
            args: [],
            type: {
              kind: 'NON_NULL',
              name: null,
              ofType: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'isDeprecated',
            description: null,
            args: [],
            type: {
              kind: 'NON_NULL',
              name: null,
              ofType: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'name',
            description: null,
            args: [],
            type: {
              kind: 'NON_NULL',
              name: null,
              ofType: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'type',
            description: null,
            args: [],
            type: {
              kind: 'OBJECT',
              name: '__Type',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
        ],
        inputFields: null,
        interfaces: [],
        enumValues: null,
        possibleTypes: null,
      },
      {
        kind: 'OBJECT',
        name: '__InputValue',
        description: null,
        fields: [
          {
            name: 'defaultValue',
            description: null,
            args: [],
            type: {
              kind: 'NON_NULL',
              name: null,
              ofType: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'description',
            description: null,
            args: [],
            type: {
              kind: 'NON_NULL',
              name: null,
              ofType: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'name',
            description: null,
            args: [],
            type: {
              kind: 'NON_NULL',
              name: null,
              ofType: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'type',
            description: null,
            args: [],
            type: {
              kind: 'OBJECT',
              name: '__Type',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
        ],
        inputFields: null,
        interfaces: [],
        enumValues: null,
        possibleTypes: null,
      },
      {
        kind: 'OBJECT',
        name: '__Schema',
        description: null,
        fields: [
          {
            name: 'description',
            description: null,
            args: [],
            type: {
              kind: 'NON_NULL',
              name: null,
              ofType: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'directives',
            description: null,
            args: [],
            type: {
              kind: 'OBJECT',
              name: '__Directive',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'mutationType',
            description: null,
            args: [],
            type: {
              kind: 'OBJECT',
              name: '__Type',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'queryType',
            description: null,
            args: [],
            type: {
              kind: 'OBJECT',
              name: '__Type',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'subscriptionType',
            description: null,
            args: [],
            type: {
              kind: 'OBJECT',
              name: '__Type',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'types',
            description: null,
            args: [],
            type: {
              kind: 'OBJECT',
              name: '__Type',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
        ],
        inputFields: null,
        interfaces: [],
        enumValues: null,
        possibleTypes: null,
      },
      {
        kind: 'OBJECT',
        name: '__Type',
        description: null,
        fields: [
          {
            name: 'description',
            description: null,
            args: [],
            type: {
              kind: 'NON_NULL',
              name: null,
              ofType: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'enumValues',
            description: null,
            args: [
              {
                name: 'includeDeprecated',
                description: null,
                type: {
                  kind: 'SCALAR',
                  name: 'Boolean',
                  ofType: null,
                },
                defaultValue: 'false',
              },
            ],
            type: {
              kind: 'OBJECT',
              name: '__EnumValue',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'fields',
            description: null,
            args: [
              {
                name: 'includeDeprecated',
                description: null,
                type: {
                  kind: 'SCALAR',
                  name: 'Boolean',
                  ofType: null,
                },
                defaultValue: 'false',
              },
            ],
            type: {
              kind: 'OBJECT',
              name: '__Field',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'inputFields',
            description: null,
            args: [],
            type: {
              kind: 'OBJECT',
              name: '__InputValue',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'interfaces',
            description: null,
            args: [],
            type: {
              kind: 'OBJECT',
              name: '__Type',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'kind',
            description: null,
            args: [],
            type: {
              kind: 'NON_NULL',
              name: null,
              ofType: {
                kind: 'ENUM',
                name: '__TypeKind',
                ofType: null,
              },
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'name',
            description: null,
            args: [],
            type: {
              kind: 'NON_NULL',
              name: null,
              ofType: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'ofType',
            description: null,
            args: [],
            type: {
              kind: 'OBJECT',
              name: '__Type',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'possibleTypes',
            description: null,
            args: [],
            type: {
              kind: 'OBJECT',
              name: '__Type',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
        ],
        inputFields: null,
        interfaces: [],
        enumValues: null,
        possibleTypes: null,
      },
      {
        kind: 'ENUM',
        name: '__TypeKind',
        description: null,
        fields: null,
        inputFields: null,
        interfaces: null,
        enumValues: [
          {
            name: 'ENUM',
            description: null,
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'INPUT_OBJECT',
            description: null,
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'INTERFACE',
            description: null,
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'LIST',
            description: null,
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'NON_NULL',
            description: null,
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'OBJECT',
            description: null,
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'SCALAR',
            description: null,
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'UNION',
            description: null,
            isDeprecated: false,
            deprecationReason: null,
          },
        ],
        possibleTypes: null,
      },
      {
        kind: 'OBJECT',
        name: 'mutation_root',
        description: 'mutation root',
        fields: [
          {
            name: 'deleteNewSchemaUserTable',
            description: 'delete data from the table: "new_schema.user_table"',
            args: [
              {
                name: 'where',
                description: 'filter the rows which have to be deleted',
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'INPUT_OBJECT',
                    name: 'NewSchemaUserTableBoolExp',
                    ofType: null,
                  },
                },
                defaultValue: null,
              },
            ],
            type: {
              kind: 'OBJECT',
              name: 'NewSchemaUserTableMutationResponse',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'deleteNewSchemaUserTableByPk',
            description:
              'delete single row from the table: "new_schema.user_table"',
            args: [
              {
                name: 'id',
                description: null,
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'SCALAR',
                    name: 'Int',
                    ofType: null,
                  },
                },
                defaultValue: null,
              },
            ],
            type: {
              kind: 'OBJECT',
              name: 'NewSchemaUserTable',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'deleteTable',
            description: 'delete data from the table: "table"',
            args: [
              {
                name: 'where',
                description: 'filter the rows which have to be deleted',
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'INPUT_OBJECT',
                    name: 'TableBoolExp',
                    ofType: null,
                  },
                },
                defaultValue: null,
              },
            ],
            type: {
              kind: 'OBJECT',
              name: 'TableMutationResponse',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'deleteTableByPk',
            description: 'delete single row from the table: "table"',
            args: [
              {
                name: 'id',
                description: null,
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'SCALAR',
                    name: 'Int',
                    ofType: null,
                  },
                },
                defaultValue: null,
              },
            ],
            type: {
              kind: 'OBJECT',
              name: 'Table',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'insertNewSchemaUserTable',
            description: 'insert data into the table: "new_schema.user_table"',
            args: [
              {
                name: 'objects',
                description: 'the rows to be inserted',
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'LIST',
                    name: null,
                    ofType: {
                      kind: 'NON_NULL',
                      name: null,
                      ofType: {
                        kind: 'INPUT_OBJECT',
                        name: 'NewSchemaUserTableInsertInput',
                        ofType: null,
                      },
                    },
                  },
                },
                defaultValue: null,
              },
              {
                name: 'onConflict',
                description: 'upsert condition',
                type: {
                  kind: 'INPUT_OBJECT',
                  name: 'NewSchemaUserTableOnConflict',
                  ofType: null,
                },
                defaultValue: null,
              },
            ],
            type: {
              kind: 'OBJECT',
              name: 'NewSchemaUserTableMutationResponse',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'insertNewSchemaUserTableOne',
            description:
              'insert a single row into the table: "new_schema.user_table"',
            args: [
              {
                name: 'object',
                description: 'the row to be inserted',
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'INPUT_OBJECT',
                    name: 'NewSchemaUserTableInsertInput',
                    ofType: null,
                  },
                },
                defaultValue: null,
              },
              {
                name: 'onConflict',
                description: 'upsert condition',
                type: {
                  kind: 'INPUT_OBJECT',
                  name: 'NewSchemaUserTableOnConflict',
                  ofType: null,
                },
                defaultValue: null,
              },
            ],
            type: {
              kind: 'OBJECT',
              name: 'NewSchemaUserTable',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'insertTable',
            description: 'insert data into the table: "table"',
            args: [
              {
                name: 'objects',
                description: 'the rows to be inserted',
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'LIST',
                    name: null,
                    ofType: {
                      kind: 'NON_NULL',
                      name: null,
                      ofType: {
                        kind: 'INPUT_OBJECT',
                        name: 'TableInsertInput',
                        ofType: null,
                      },
                    },
                  },
                },
                defaultValue: null,
              },
              {
                name: 'onConflict',
                description: 'upsert condition',
                type: {
                  kind: 'INPUT_OBJECT',
                  name: 'TableOnConflict',
                  ofType: null,
                },
                defaultValue: null,
              },
            ],
            type: {
              kind: 'OBJECT',
              name: 'TableMutationResponse',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'insertTableOne',
            description: 'insert a single row into the table: "table"',
            args: [
              {
                name: 'object',
                description: 'the row to be inserted',
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'INPUT_OBJECT',
                    name: 'TableInsertInput',
                    ofType: null,
                  },
                },
                defaultValue: null,
              },
              {
                name: 'onConflict',
                description: 'upsert condition',
                type: {
                  kind: 'INPUT_OBJECT',
                  name: 'TableOnConflict',
                  ofType: null,
                },
                defaultValue: null,
              },
            ],
            type: {
              kind: 'OBJECT',
              name: 'Table',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'updateNewSchemaUserTable',
            description: 'update data of the table: "new_schema.user_table"',
            args: [
              {
                name: '_inc',
                description:
                  'increments the numeric columns with given value of the filtered values',
                type: {
                  kind: 'INPUT_OBJECT',
                  name: 'NewSchemaUserTableIncInput',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: '_set',
                description:
                  'sets the columns of the filtered rows to the given values',
                type: {
                  kind: 'INPUT_OBJECT',
                  name: 'NewSchemaUserTableSetInput',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: 'where',
                description: 'filter the rows which have to be updated',
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'INPUT_OBJECT',
                    name: 'NewSchemaUserTableBoolExp',
                    ofType: null,
                  },
                },
                defaultValue: null,
              },
            ],
            type: {
              kind: 'OBJECT',
              name: 'NewSchemaUserTableMutationResponse',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'updateNewSchemaUserTableByPk',
            description:
              'update single row of the table: "new_schema.user_table"',
            args: [
              {
                name: '_inc',
                description:
                  'increments the numeric columns with given value of the filtered values',
                type: {
                  kind: 'INPUT_OBJECT',
                  name: 'NewSchemaUserTableIncInput',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: '_set',
                description:
                  'sets the columns of the filtered rows to the given values',
                type: {
                  kind: 'INPUT_OBJECT',
                  name: 'NewSchemaUserTableSetInput',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: 'pk_columns',
                description: null,
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'INPUT_OBJECT',
                    name: 'NewSchemaUserTablePkColumnsInput',
                    ofType: null,
                  },
                },
                defaultValue: null,
              },
            ],
            type: {
              kind: 'OBJECT',
              name: 'NewSchemaUserTable',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'updateNewSchemaUserTableMany',
            description:
              'update multiples rows of table: "new_schema.user_table"',
            args: [
              {
                name: 'updates',
                description: 'updates to execute, in order',
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'LIST',
                    name: null,
                    ofType: {
                      kind: 'NON_NULL',
                      name: null,
                      ofType: {
                        kind: 'INPUT_OBJECT',
                        name: 'NewSchemaUserTableUpdates',
                        ofType: null,
                      },
                    },
                  },
                },
                defaultValue: null,
              },
            ],
            type: {
              kind: 'LIST',
              name: null,
              ofType: {
                kind: 'OBJECT',
                name: 'NewSchemaUserTableMutationResponse',
                ofType: null,
              },
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'updateTable',
            description: 'update data of the table: "table"',
            args: [
              {
                name: '_inc',
                description:
                  'increments the numeric columns with given value of the filtered values',
                type: {
                  kind: 'INPUT_OBJECT',
                  name: 'TableIncInput',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: '_set',
                description:
                  'sets the columns of the filtered rows to the given values',
                type: {
                  kind: 'INPUT_OBJECT',
                  name: 'TableSetInput',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: 'where',
                description: 'filter the rows which have to be updated',
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'INPUT_OBJECT',
                    name: 'TableBoolExp',
                    ofType: null,
                  },
                },
                defaultValue: null,
              },
            ],
            type: {
              kind: 'OBJECT',
              name: 'TableMutationResponse',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'updateTableByPk',
            description: 'update single row of the table: "table"',
            args: [
              {
                name: '_inc',
                description:
                  'increments the numeric columns with given value of the filtered values',
                type: {
                  kind: 'INPUT_OBJECT',
                  name: 'TableIncInput',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: '_set',
                description:
                  'sets the columns of the filtered rows to the given values',
                type: {
                  kind: 'INPUT_OBJECT',
                  name: 'TableSetInput',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: 'pk_columns',
                description: null,
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'INPUT_OBJECT',
                    name: 'TablePkColumnsInput',
                    ofType: null,
                  },
                },
                defaultValue: null,
              },
            ],
            type: {
              kind: 'OBJECT',
              name: 'Table',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'updateTableMany',
            description: 'update multiples rows of table: "table"',
            args: [
              {
                name: 'updates',
                description: 'updates to execute, in order',
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'LIST',
                    name: null,
                    ofType: {
                      kind: 'NON_NULL',
                      name: null,
                      ofType: {
                        kind: 'INPUT_OBJECT',
                        name: 'TableUpdates',
                        ofType: null,
                      },
                    },
                  },
                },
                defaultValue: null,
              },
            ],
            type: {
              kind: 'LIST',
              name: null,
              ofType: {
                kind: 'OBJECT',
                name: 'TableMutationResponse',
                ofType: null,
              },
            },
            isDeprecated: false,
            deprecationReason: null,
          },
        ],
        inputFields: null,
        interfaces: [],
        enumValues: null,
        possibleTypes: null,
      },
      {
        kind: 'INPUT_OBJECT',
        name: 'new_schema_user_table_streamCursorInput',
        description: 'Streaming cursor of the table "new_schema_user_table"',
        fields: null,
        inputFields: [
          {
            name: 'initialValue',
            description: 'Stream column input with initial value',
            type: {
              kind: 'NON_NULL',
              name: null,
              ofType: {
                kind: 'INPUT_OBJECT',
                name: 'new_schema_user_table_streamCursorValueInput',
                ofType: null,
              },
            },
            defaultValue: null,
          },
          {
            name: 'ordering',
            description: 'cursor ordering',
            type: {
              kind: 'ENUM',
              name: 'CursorOrdering',
              ofType: null,
            },
            defaultValue: null,
          },
        ],
        interfaces: null,
        enumValues: null,
        possibleTypes: null,
      },
      {
        kind: 'INPUT_OBJECT',
        name: 'new_schema_user_table_streamCursorValueInput',
        description:
          'Initial value of the column from where the streaming should start',
        fields: null,
        inputFields: [
          {
            name: 'id',
            description: null,
            type: {
              kind: 'SCALAR',
              name: 'Int',
              ofType: null,
            },
            defaultValue: null,
          },
          {
            name: 'name',
            description: null,
            type: {
              kind: 'SCALAR',
              name: 'String',
              ofType: null,
            },
            defaultValue: null,
          },
        ],
        interfaces: null,
        enumValues: null,
        possibleTypes: null,
      },
      {
        kind: 'OBJECT',
        name: 'query_root',
        description: null,
        fields: [
          {
            name: 'newSchemaUserTable',
            description: 'fetch data from the table: "new_schema.user_table"',
            args: [
              {
                name: 'distinctOn',
                description: 'distinct select on columns',
                type: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'ENUM',
                      name: 'NewSchemaUserTableSelectColumn',
                      ofType: null,
                    },
                  },
                },
                defaultValue: null,
              },
              {
                name: 'limit',
                description: 'limit the number of rows returned',
                type: {
                  kind: 'SCALAR',
                  name: 'Int',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: 'offset',
                description: 'skip the first n rows. Use only with order_by',
                type: {
                  kind: 'SCALAR',
                  name: 'Int',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: 'orderBy',
                description: 'sort the rows by one or more columns',
                type: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'NewSchemaUserTableOrderBy',
                      ofType: null,
                    },
                  },
                },
                defaultValue: null,
              },
              {
                name: 'where',
                description: 'filter the rows returned',
                type: {
                  kind: 'INPUT_OBJECT',
                  name: 'NewSchemaUserTableBoolExp',
                  ofType: null,
                },
                defaultValue: null,
              },
            ],
            type: {
              kind: 'NON_NULL',
              name: null,
              ofType: {
                kind: 'LIST',
                name: null,
                ofType: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'OBJECT',
                    name: 'NewSchemaUserTable',
                    ofType: null,
                  },
                },
              },
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'newSchemaUserTableAggregate',
            description:
              'fetch aggregated fields from the table: "new_schema.user_table"',
            args: [
              {
                name: 'distinctOn',
                description: 'distinct select on columns',
                type: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'ENUM',
                      name: 'NewSchemaUserTableSelectColumn',
                      ofType: null,
                    },
                  },
                },
                defaultValue: null,
              },
              {
                name: 'limit',
                description: 'limit the number of rows returned',
                type: {
                  kind: 'SCALAR',
                  name: 'Int',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: 'offset',
                description: 'skip the first n rows. Use only with order_by',
                type: {
                  kind: 'SCALAR',
                  name: 'Int',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: 'orderBy',
                description: 'sort the rows by one or more columns',
                type: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'NewSchemaUserTableOrderBy',
                      ofType: null,
                    },
                  },
                },
                defaultValue: null,
              },
              {
                name: 'where',
                description: 'filter the rows returned',
                type: {
                  kind: 'INPUT_OBJECT',
                  name: 'NewSchemaUserTableBoolExp',
                  ofType: null,
                },
                defaultValue: null,
              },
            ],
            type: {
              kind: 'NON_NULL',
              name: null,
              ofType: {
                kind: 'OBJECT',
                name: 'NewSchemaUserTableAggregate',
                ofType: null,
              },
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'newSchemaUserTableByPk',
            description:
              'fetch data from the table: "new_schema.user_table" using primary key columns',
            args: [
              {
                name: 'id',
                description: null,
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'SCALAR',
                    name: 'Int',
                    ofType: null,
                  },
                },
                defaultValue: null,
              },
            ],
            type: {
              kind: 'OBJECT',
              name: 'NewSchemaUserTable',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'table',
            description: 'fetch data from the table: "table"',
            args: [
              {
                name: 'distinctOn',
                description: 'distinct select on columns',
                type: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'ENUM',
                      name: 'TableSelectColumn',
                      ofType: null,
                    },
                  },
                },
                defaultValue: null,
              },
              {
                name: 'limit',
                description: 'limit the number of rows returned',
                type: {
                  kind: 'SCALAR',
                  name: 'Int',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: 'offset',
                description: 'skip the first n rows. Use only with order_by',
                type: {
                  kind: 'SCALAR',
                  name: 'Int',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: 'orderBy',
                description: 'sort the rows by one or more columns',
                type: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'TableOrderBy',
                      ofType: null,
                    },
                  },
                },
                defaultValue: null,
              },
              {
                name: 'where',
                description: 'filter the rows returned',
                type: {
                  kind: 'INPUT_OBJECT',
                  name: 'TableBoolExp',
                  ofType: null,
                },
                defaultValue: null,
              },
            ],
            type: {
              kind: 'NON_NULL',
              name: null,
              ofType: {
                kind: 'LIST',
                name: null,
                ofType: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'OBJECT',
                    name: 'Table',
                    ofType: null,
                  },
                },
              },
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'tableAggregate',
            description: 'fetch aggregated fields from the table: "table"',
            args: [
              {
                name: 'distinctOn',
                description: 'distinct select on columns',
                type: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'ENUM',
                      name: 'TableSelectColumn',
                      ofType: null,
                    },
                  },
                },
                defaultValue: null,
              },
              {
                name: 'limit',
                description: 'limit the number of rows returned',
                type: {
                  kind: 'SCALAR',
                  name: 'Int',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: 'offset',
                description: 'skip the first n rows. Use only with order_by',
                type: {
                  kind: 'SCALAR',
                  name: 'Int',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: 'orderBy',
                description: 'sort the rows by one or more columns',
                type: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'TableOrderBy',
                      ofType: null,
                    },
                  },
                },
                defaultValue: null,
              },
              {
                name: 'where',
                description: 'filter the rows returned',
                type: {
                  kind: 'INPUT_OBJECT',
                  name: 'TableBoolExp',
                  ofType: null,
                },
                defaultValue: null,
              },
            ],
            type: {
              kind: 'NON_NULL',
              name: null,
              ofType: {
                kind: 'OBJECT',
                name: 'TableAggregate',
                ofType: null,
              },
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'tableByPk',
            description:
              'fetch data from the table: "table" using primary key columns',
            args: [
              {
                name: 'id',
                description: null,
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'SCALAR',
                    name: 'Int',
                    ofType: null,
                  },
                },
                defaultValue: null,
              },
            ],
            type: {
              kind: 'OBJECT',
              name: 'Table',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
        ],
        inputFields: null,
        interfaces: [],
        enumValues: null,
        possibleTypes: null,
      },
      {
        kind: 'OBJECT',
        name: 'subscription_root',
        description: null,
        fields: [
          {
            name: 'newSchemaUserTable',
            description: 'fetch data from the table: "new_schema.user_table"',
            args: [
              {
                name: 'distinctOn',
                description: 'distinct select on columns',
                type: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'ENUM',
                      name: 'NewSchemaUserTableSelectColumn',
                      ofType: null,
                    },
                  },
                },
                defaultValue: null,
              },
              {
                name: 'limit',
                description: 'limit the number of rows returned',
                type: {
                  kind: 'SCALAR',
                  name: 'Int',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: 'offset',
                description: 'skip the first n rows. Use only with order_by',
                type: {
                  kind: 'SCALAR',
                  name: 'Int',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: 'orderBy',
                description: 'sort the rows by one or more columns',
                type: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'NewSchemaUserTableOrderBy',
                      ofType: null,
                    },
                  },
                },
                defaultValue: null,
              },
              {
                name: 'where',
                description: 'filter the rows returned',
                type: {
                  kind: 'INPUT_OBJECT',
                  name: 'NewSchemaUserTableBoolExp',
                  ofType: null,
                },
                defaultValue: null,
              },
            ],
            type: {
              kind: 'NON_NULL',
              name: null,
              ofType: {
                kind: 'LIST',
                name: null,
                ofType: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'OBJECT',
                    name: 'NewSchemaUserTable',
                    ofType: null,
                  },
                },
              },
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'newSchemaUserTableAggregate',
            description:
              'fetch aggregated fields from the table: "new_schema.user_table"',
            args: [
              {
                name: 'distinctOn',
                description: 'distinct select on columns',
                type: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'ENUM',
                      name: 'NewSchemaUserTableSelectColumn',
                      ofType: null,
                    },
                  },
                },
                defaultValue: null,
              },
              {
                name: 'limit',
                description: 'limit the number of rows returned',
                type: {
                  kind: 'SCALAR',
                  name: 'Int',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: 'offset',
                description: 'skip the first n rows. Use only with order_by',
                type: {
                  kind: 'SCALAR',
                  name: 'Int',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: 'orderBy',
                description: 'sort the rows by one or more columns',
                type: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'NewSchemaUserTableOrderBy',
                      ofType: null,
                    },
                  },
                },
                defaultValue: null,
              },
              {
                name: 'where',
                description: 'filter the rows returned',
                type: {
                  kind: 'INPUT_OBJECT',
                  name: 'NewSchemaUserTableBoolExp',
                  ofType: null,
                },
                defaultValue: null,
              },
            ],
            type: {
              kind: 'NON_NULL',
              name: null,
              ofType: {
                kind: 'OBJECT',
                name: 'NewSchemaUserTableAggregate',
                ofType: null,
              },
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'newSchemaUserTableByPk',
            description:
              'fetch data from the table: "new_schema.user_table" using primary key columns',
            args: [
              {
                name: 'id',
                description: null,
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'SCALAR',
                    name: 'Int',
                    ofType: null,
                  },
                },
                defaultValue: null,
              },
            ],
            type: {
              kind: 'OBJECT',
              name: 'NewSchemaUserTable',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'newSchemaUserTableStream',
            description:
              'fetch data from the table in a streaming manner: "new_schema.user_table"',
            args: [
              {
                name: 'batchSize',
                description:
                  'maximum number of rows returned in a single batch',
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'SCALAR',
                    name: 'Int',
                    ofType: null,
                  },
                },
                defaultValue: null,
              },
              {
                name: 'cursor',
                description:
                  'cursor to stream the results returned by the query',
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'LIST',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'new_schema_user_table_streamCursorInput',
                      ofType: null,
                    },
                  },
                },
                defaultValue: null,
              },
              {
                name: 'where',
                description: 'filter the rows returned',
                type: {
                  kind: 'INPUT_OBJECT',
                  name: 'NewSchemaUserTableBoolExp',
                  ofType: null,
                },
                defaultValue: null,
              },
            ],
            type: {
              kind: 'NON_NULL',
              name: null,
              ofType: {
                kind: 'LIST',
                name: null,
                ofType: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'OBJECT',
                    name: 'NewSchemaUserTable',
                    ofType: null,
                  },
                },
              },
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'table',
            description: 'fetch data from the table: "table"',
            args: [
              {
                name: 'distinctOn',
                description: 'distinct select on columns',
                type: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'ENUM',
                      name: 'TableSelectColumn',
                      ofType: null,
                    },
                  },
                },
                defaultValue: null,
              },
              {
                name: 'limit',
                description: 'limit the number of rows returned',
                type: {
                  kind: 'SCALAR',
                  name: 'Int',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: 'offset',
                description: 'skip the first n rows. Use only with order_by',
                type: {
                  kind: 'SCALAR',
                  name: 'Int',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: 'orderBy',
                description: 'sort the rows by one or more columns',
                type: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'TableOrderBy',
                      ofType: null,
                    },
                  },
                },
                defaultValue: null,
              },
              {
                name: 'where',
                description: 'filter the rows returned',
                type: {
                  kind: 'INPUT_OBJECT',
                  name: 'TableBoolExp',
                  ofType: null,
                },
                defaultValue: null,
              },
            ],
            type: {
              kind: 'NON_NULL',
              name: null,
              ofType: {
                kind: 'LIST',
                name: null,
                ofType: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'OBJECT',
                    name: 'Table',
                    ofType: null,
                  },
                },
              },
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'tableAggregate',
            description: 'fetch aggregated fields from the table: "table"',
            args: [
              {
                name: 'distinctOn',
                description: 'distinct select on columns',
                type: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'ENUM',
                      name: 'TableSelectColumn',
                      ofType: null,
                    },
                  },
                },
                defaultValue: null,
              },
              {
                name: 'limit',
                description: 'limit the number of rows returned',
                type: {
                  kind: 'SCALAR',
                  name: 'Int',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: 'offset',
                description: 'skip the first n rows. Use only with order_by',
                type: {
                  kind: 'SCALAR',
                  name: 'Int',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: 'orderBy',
                description: 'sort the rows by one or more columns',
                type: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'TableOrderBy',
                      ofType: null,
                    },
                  },
                },
                defaultValue: null,
              },
              {
                name: 'where',
                description: 'filter the rows returned',
                type: {
                  kind: 'INPUT_OBJECT',
                  name: 'TableBoolExp',
                  ofType: null,
                },
                defaultValue: null,
              },
            ],
            type: {
              kind: 'NON_NULL',
              name: null,
              ofType: {
                kind: 'OBJECT',
                name: 'TableAggregate',
                ofType: null,
              },
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'tableByPk',
            description:
              'fetch data from the table: "table" using primary key columns',
            args: [
              {
                name: 'id',
                description: null,
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'SCALAR',
                    name: 'Int',
                    ofType: null,
                  },
                },
                defaultValue: null,
              },
            ],
            type: {
              kind: 'OBJECT',
              name: 'Table',
              ofType: null,
            },
            isDeprecated: false,
            deprecationReason: null,
          },
          {
            name: 'tableStream',
            description:
              'fetch data from the table in a streaming manner: "table"',
            args: [
              {
                name: 'batchSize',
                description:
                  'maximum number of rows returned in a single batch',
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'SCALAR',
                    name: 'Int',
                    ofType: null,
                  },
                },
                defaultValue: null,
              },
              {
                name: 'cursor',
                description:
                  'cursor to stream the results returned by the query',
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'LIST',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'table_streamCursorInput',
                      ofType: null,
                    },
                  },
                },
                defaultValue: null,
              },
              {
                name: 'where',
                description: 'filter the rows returned',
                type: {
                  kind: 'INPUT_OBJECT',
                  name: 'TableBoolExp',
                  ofType: null,
                },
                defaultValue: null,
              },
            ],
            type: {
              kind: 'NON_NULL',
              name: null,
              ofType: {
                kind: 'LIST',
                name: null,
                ofType: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'OBJECT',
                    name: 'Table',
                    ofType: null,
                  },
                },
              },
            },
            isDeprecated: false,
            deprecationReason: null,
          },
        ],
        inputFields: null,
        interfaces: [],
        enumValues: null,
        possibleTypes: null,
      },
      {
        kind: 'INPUT_OBJECT',
        name: 'table_streamCursorInput',
        description: 'Streaming cursor of the table "table"',
        fields: null,
        inputFields: [
          {
            name: 'initialValue',
            description: 'Stream column input with initial value',
            type: {
              kind: 'NON_NULL',
              name: null,
              ofType: {
                kind: 'INPUT_OBJECT',
                name: 'table_streamCursorValueInput',
                ofType: null,
              },
            },
            defaultValue: null,
          },
          {
            name: 'ordering',
            description: 'cursor ordering',
            type: {
              kind: 'ENUM',
              name: 'CursorOrdering',
              ofType: null,
            },
            defaultValue: null,
          },
        ],
        interfaces: null,
        enumValues: null,
        possibleTypes: null,
      },
      {
        kind: 'INPUT_OBJECT',
        name: 'table_streamCursorValueInput',
        description:
          'Initial value of the column from where the streaming should start',
        fields: null,
        inputFields: [
          {
            name: 'id',
            description: null,
            type: {
              kind: 'SCALAR',
              name: 'Int',
              ofType: null,
            },
            defaultValue: null,
          },
          {
            name: 'name',
            description: null,
            type: {
              kind: 'SCALAR',
              name: 'String',
              ofType: null,
            },
            defaultValue: null,
          },
        ],
        interfaces: null,
        enumValues: null,
        possibleTypes: null,
      },
    ],
    directives: [
      {
        name: 'include',
        description: 'whether this query should be included',
        locations: ['FIELD', 'FRAGMENT_SPREAD', 'INLINE_FRAGMENT'],
        args: [
          {
            name: 'if',
            description: null,
            type: {
              kind: 'NON_NULL',
              name: null,
              ofType: {
                kind: 'SCALAR',
                name: 'Boolean',
                ofType: null,
              },
            },
            defaultValue: null,
          },
        ],
      },
      {
        name: 'skip',
        description: 'whether this query should be skipped',
        locations: ['FIELD', 'FRAGMENT_SPREAD', 'INLINE_FRAGMENT'],
        args: [
          {
            name: 'if',
            description: null,
            type: {
              kind: 'NON_NULL',
              name: null,
              ofType: {
                kind: 'SCALAR',
                name: 'Boolean',
                ofType: null,
              },
            },
            defaultValue: null,
          },
        ],
      },
      {
        name: 'cached',
        description: 'whether this query should be cached (Hasura Cloud only)',
        locations: ['QUERY'],
        args: [
          {
            name: 'ttl',
            description: 'measured in seconds',
            type: {
              kind: 'NON_NULL',
              name: null,
              ofType: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
            },
            defaultValue: '60',
          },
          {
            name: 'refresh',
            description: 'refresh the cache entry',
            type: {
              kind: 'NON_NULL',
              name: null,
              ofType: {
                kind: 'SCALAR',
                name: 'Boolean',
                ofType: null,
              },
            },
            defaultValue: 'false',
          },
        ],
      },
    ],
  },
} as any;
