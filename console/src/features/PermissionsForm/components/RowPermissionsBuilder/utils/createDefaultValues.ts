import { MetadataTable } from '@/features/MetadataAPI';
import { GraphQLSchema } from 'graphql';
import { getAllColumnsAndOperators } from '.';

export interface CreateOperatorsArgs {
  tableName: string;
  schema?: GraphQLSchema;
  existingPermission?: Record<string, any>;
  tableConfig: MetadataTable['configuration'];
}

export const createOperatorsObject = ({
  tableName,
  schema,
  existingPermission,
  tableConfig,
}: CreateOperatorsArgs): Record<string, any> => {
  if (!existingPermission || !schema) {
    return {};
  }
  const data = getAllColumnsAndOperators({ tableName, schema, tableConfig });

  const colNames = data.columns.map(col => col.name);
  const boolOperators = data.boolOperators.map(bo => bo.name);
  const relationships = data.relationships.map(rel => rel.name);

  const operators = Object.entries(existingPermission).reduce(
    (_acc, [key, value]) => {
      if (boolOperators.includes(key)) {
        return {
          name: key,
          typeName: key,
          type: 'boolOperator',
          [key]: value.map((each: Record<string, any>) =>
            createOperatorsObject({
              tableName,
              schema,
              existingPermission: each,
              tableConfig,
            })
          ),
        };
      }

      if (relationships.includes(key)) {
        const rel = data.relationships.find(r => key === r.name);
        const typeName = rel?.meta?.type?.type;

        return {
          name: key,
          typeName,
          type: 'relationship',
          [key]: createOperatorsObject({
            tableName: typeName || '',
            schema,
            existingPermission: value,
            tableConfig,
          }),
        };
      }

      if (colNames.includes(key)) {
        return {
          name: key,
          typeName: key,
          type: 'column',
          columnOperator: createOperatorsObject({
            tableName,
            schema,
            existingPermission: value,
            tableConfig,
          }),
        };
      }

      return key;
    },
    {}
  );

  return operators;
};

export interface CreateDefaultsArgs {
  tableName: string;
  schema?: GraphQLSchema;
  existingPermission?: Record<string, any>;
  tableConfig: MetadataTable['configuration'];
}

export const createDefaultValues = (props: CreateDefaultsArgs) => {
  const { tableName, schema, existingPermission, tableConfig } = props;
  if (!existingPermission) {
    return {};
  }

  const operators = createOperatorsObject({
    tableName,
    schema,
    existingPermission,
    tableConfig,
  });

  return {
    operators: {
      filter: operators,
    },
    filter: existingPermission,
  };
};
