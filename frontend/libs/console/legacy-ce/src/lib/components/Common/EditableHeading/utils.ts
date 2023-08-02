import { Table, TableColumn } from '../../../dataSources/types';
import moment from 'moment';
import { capitaliseFirstLetter } from '../ConfigureTransformation/utils';

const indentFields = (subFields: TableColumn[], depth: number) =>
  subFields
    .map(sf => sf.column_name)
    .join(`\n${Array(depth).fill('\t').join('')}`)
    .trim();

const toPascalCase = (str: string) =>
  str.split(/[-_]/).map(capitaliseFirstLetter).join('');

const subFieldsTypeMapping: Record<
  string,
  { type: string; default: () => unknown }
> = {
  integer: {
    type: 'Int',
    default: () => 0,
  },
  text: {
    type: 'String',
    default: () => '',
  },
  boolean: {
    type: 'Boolean',
    default: () => true,
  },
  numeric: {
    type: 'Float',
    default: () => 0,
  },

  'timestamp with time zone': {
    type: 'String',
    default: () => moment().format(),
  },
  'time with time zone': {
    type: 'String',
    default: () => moment().format('HH:mm:ss.SSSSSSZZ'),
  },
  date: {
    type: 'String',
    default: () => moment().format('YYYY-MM-DD'),
  },
  uuid: {
    type: 'String',
    default: () => '00000000-0000-0000-0000-000000000000',
  },
  jsonb: {
    type: 'String',
    default: () => '{}',
  },
  bigint: {
    type: 'Int',
    default: () => 0,
  },
};

export const getInitialValueField = (table: Table): TableColumn | undefined =>
  table.columns.find(
    f =>
      (table.primary_key?.columns || []).includes(f.column_name) &&
      ['bigint', 'integer'].includes(f.data_type)
  ) ??
  table.columns.find(f =>
    ['timestamp with time zone', 'time with time zone', 'date'].includes(
      f.data_type
    )
  );

export type OperationType =
  | 'subscription'
  | 'query'
  | 'mutation'
  | 'streaming_subscription';

export const generateGqlQueryFromTable = (
  operationType: OperationType = 'subscription',
  table: Table
): {
  query: string;
  variables?: Record<string, unknown>;
} => {
  const fieldName = table.table_name;
  const fields = table.columns;
  const pascalCaseName = toPascalCase(fieldName);
  const tableSchemaPart =
    table.table_schema !== 'public' ? `${table.table_schema}_` : '';

  if (operationType === 'query') {
    const query = `query Get${pascalCaseName} {
  ${tableSchemaPart}${fieldName} {
    ${indentFields(fields, 2)}
  }
}
    `;
    return {
      query,
    };
  }

  if (operationType === 'mutation') {
    const mandatoryFields = fields.filter(
      sf => !sf.column_default && subFieldsTypeMapping[sf.data_type]
    );
    const args = mandatoryFields
      .map(
        sf => `$${sf.column_name}: ${subFieldsTypeMapping[sf.data_type].type}`
      )
      .join(', ')
      .trim();
    const argsUsage = mandatoryFields
      .map(sf => `${sf.column_name}: $${sf.column_name}`)
      .join(', ')
      .trim();

    const query = `mutation Insert${pascalCaseName}(${args}) {
  insert_${tableSchemaPart}${fieldName}(objects: {${argsUsage}}) {
    affected_rows
    returning {
      ${indentFields(fields, 3)}
    }
  }
}
    `;

    const variables = mandatoryFields.reduce(
      (acc, sf) => ({
        ...acc,
        [sf.column_name]: subFieldsTypeMapping[sf.data_type].default(),
      }),
      {}
    );

    return {
      query,
      variables,
    };
  }

  if (operationType === 'streaming_subscription') {
    const initialValueColumn = getInitialValueField(table);
    if (initialValueColumn) {
      const initialValue = JSON.stringify(
        subFieldsTypeMapping[initialValueColumn.data_type]?.default()
      );

      const query = `subscription Get${pascalCaseName}StreamingSubscription {
  ${tableSchemaPart}${fieldName}_stream(batch_size: 10, cursor: {initial_value: {${
        initialValueColumn.column_name
      }: ${initialValue}}}) {
    ${indentFields(fields, 2)}
  }
}
    `;
      return {
        query,
      };
    }
  }

  if (operationType === 'subscription') {
    const query = `subscription Get${pascalCaseName}StreamingSubscription {
  ${tableSchemaPart}${fieldName} {
    ${indentFields(fields, 2)}
  }
}
    

    `;
    return {
      query,
    };
  }

  return {
    query: '',
  };
};
