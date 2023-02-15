import { allOperators } from '@/components/Common/FilterQuery/utils';
import { GraphQLSchema, isInputObjectType } from 'graphql';
import { lowerCase } from 'lodash';
import { Comparators } from '../types';

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
    const operators = Object.values(inputType.getFields()).map(field => {
      const name = field.name;
      return {
        name: allOperators.find(o => o.alias === name)?.name ?? lowerCase(name),
        operator: name,
        type: field.type,
      };
    });
    return { ...acc, [inputType.name]: { operators } };
  }, {});
}
