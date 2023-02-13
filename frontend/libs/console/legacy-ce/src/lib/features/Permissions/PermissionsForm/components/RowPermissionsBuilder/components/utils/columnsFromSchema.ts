import { GraphQLSchema, isInputObjectType } from 'graphql';
import { Columns } from '../types';

export function columnsFromSchema(
  schema: GraphQLSchema | undefined
): Record<string, Columns> {
  if (!schema) {
    return {};
  }
  // Get input types ending in `_bool_exp`. They represent tables
  // E.g: Artist_bool_exp
  // For each table input type, get the fields that end with `_comparison_exp`
  // Return a map of table name to columns
  const inputObjectTypes = Object.values(schema.getTypeMap()).filter(
    type => isInputObjectType(type) && type.name.endsWith('_bool_exp')
  );
  return inputObjectTypes.reduce((acc, inputType) => {
    if (!isInputObjectType(inputType)) {
      return acc;
    }
    const columns: Columns = Object.values(inputType.getFields())
      .filter(field => field.type.toString().endsWith('_comparison_exp'))
      .map(field => {
        return {
          name: field.name,
          type: field.type.toString(),
          graphQLType: field.type,
        };
      });
    return { ...acc, [inputType.name.replace('_bool_exp', '')]: columns };
  }, {} as Record<string, Columns>);
}
