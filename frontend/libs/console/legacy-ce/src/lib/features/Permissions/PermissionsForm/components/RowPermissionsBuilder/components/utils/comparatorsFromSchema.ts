import { GraphQLSchema, isInputObjectType } from 'graphql';
import {
  columnOperatorsInfo,
  boolOperatorsInfo,
} from '../../../../../../../components/Services/Data/TablePermissions/PermissionBuilder/utils';
import lowerCase from 'lodash/lowerCase';
import { tableContext } from '../TableProvider';
import { Columns, Comparators, Tables, Operator } from '../types';
import { areTablesEqual } from '../../../../../../hasura-metadata-api';
import { Table } from '../../../../../../hasura-metadata-types';
import { useContext } from 'react';
import { rowPermissionsContext } from '../RowPermissionsProvider';

function columnOperators(): Array<Operator> {
  return Object.keys(columnOperatorsInfo).reduce((acc, key) => {
    const operator = (
      columnOperatorsInfo as Record<string, Omit<Operator, 'name'>>
    )[key];
    return [
      ...acc,
      {
        name: key,
        inputStructure: operator.inputStructure,
        inputType: operator.inputType,
        type: operator.type,
      },
    ];
  }, [] as Array<Operator>);
}

function boolOperators(): Array<Operator> {
  return Object.keys(boolOperatorsInfo).reduce((acc, key) => {
    const operator = (
      boolOperatorsInfo as Record<string, Omit<Operator, 'name'>>
    )[key];
    return [
      ...acc,
      {
        name: key,
        inputStructure: operator.inputStructure,
        inputType: operator.inputType,
        type: operator.type,
      },
    ];
  }, [] as Array<Operator>);
}

export const allOperators: Array<Operator> = [
  ...columnOperators(),
  ...boolOperators(),
  {
    name: '_exists',
    type: 'comparision',
  },
];

const columnComparators = [
  {
    name: '_ceq',
    operator: '_ceq',
  },
  {
    name: '_cne',
    operator: '_cne',
  },
  {
    name: '_cgt',
    operator: '_cgt',
  },
  {
    name: '_clt',
    operator: '_clt',
  },
  {
    name: '_cgte',
    operator: '_cgte',
  },
  {
    name: '_clte',
    operator: '_clte',
  },
];

export function comparatorsFromSchema(schema: GraphQLSchema): Comparators {
  // Get input types ending in `_comparison_exp`
  // E.g: String_BigQuery_comparison_exp, string_SQLite_comparison_exp, String_comparison_exp, etc...
  const inputObjectTypes = Object.values(schema.getTypeMap()).filter(
    type => isInputObjectType(type) && type.name.endsWith('_comparison_exp')
  );
  return inputObjectTypes.reduce((acc, inputType) => {
    if (!isInputObjectType(inputType)) {
      return acc;
    }
    const operators: Operator[] = Object.values(inputType.getFields()).map(
      field => {
        const name = field.name;
        const operator = allOperators.find(o => o.name === name);
        return {
          type: operator?.type || '',
          name: operator?.name ?? lowerCase(name),
          operator: name,
          graphqlType: field.type,
          inputStructure: operator?.inputStructure,
          inputType: operator?.inputType,
        };
      }
    );
    const key = inputType.name.replace('_comparison_exp', '');
    return {
      ...acc,
      [key]: {
        operators: [
          ...operators,
          ...columnComparators, // Add column comparators because they are not reflected on the graphql schema
        ],
      },
    };
  }, {});
}

const whitelist: Record<string, string[]> = {
  jsonb: [
    '_is_null',
    '_contains',
    '_contained_in',
    '_has_key',
    '_has_keys_any',
    '_has_keys_all',
  ],
  // JSON does not seem to come with any operators
  // To match the old implementation, which does not provide any operators, we do not whitelist any for now
  json: [],
  geography: ['_st_d_within', '_is_null', '_st_intersects'],
  geometry: [
    '_is_null',
    '_st_d_within',
    '_st_within',
    '_st_3d_d_within',
    '_st_contains',
    '_st_intersects',
    '_st_touches',
    '_st_overlaps',
    '_st_crosses',
  ],
};

export function useOperators({ path }: { path: string[] }) {
  const { comparators, tables } = useContext(rowPermissionsContext);
  const { columns, table } = useContext(tableContext);
  const columnName = path[path.length - 2];
  const column = columns.find(c => c.name === columnName);
  let dataType = column?.dataType;
  if (dataType === 'USER-DEFINED') {
    // eslint-disable-next-line @typescript-eslint/ban-ts-comment
    // @ts-ignore
    dataType = column?.graphQLProperties?.scalarType;
  }
  const operators = getDataTypeOperators({
    comparators,
    path,
    columns,
    tables,
    table,
  });
  if (dataType && hasWhitelistedOperators(dataType)) {
    return operators.filter(o => whitelist[dataType || '']?.includes(o.name));
  }
  return operators;
}

function getDataTypeOperators({
  comparators,
  path,
  columns,
  tables,
  table,
}: {
  comparators: Comparators;
  path: string[];
  columns: Columns;
  tables: Tables;
  table: Table;
}) {
  const columnName = path[path.length - 2];
  const column = columns.find(c => c.name === columnName);
  const dataSourceKind = tables.find(t => areTablesEqual(t.table, table))
    ?.dataSource?.kind;
  // types associated to postgres don't have a suffix on the GraphQL Schema, whereas the other types have dataSource as suffix
  const comparatorSuffix =
    dataSourceKind === 'postgres' ? '' : `_${dataSourceKind}`;
  const comparatorKey = column ? `${column.dataType}${comparatorSuffix}` : '';
  const operators = comparators[comparatorKey]?.operators;
  if (!operators) {
    return allOperators;
  }
  return operators;
}

function hasWhitelistedOperators(dataType: string) {
  return whitelist[dataType];
}
