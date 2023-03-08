import { allOperators } from '../../../../../../../components/Common/FilterQuery/utils';
import { GraphQLSchema, isInputObjectType } from 'graphql';
import { lowerCase } from 'lodash';
import { Comparators } from '../types';

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
    const operators = Object.values(inputType.getFields()).map(field => {
      const name = field.name;
      return {
        name:
          allOperators.find(o => o.alias === name)?.alias ?? lowerCase(name),
        operator: name,
        type: field.type,
      };
    });
    return {
      ...acc,
      [inputType.name]: {
        // Add column comparators because they are not reflected on the graphql schema
        operators: [...operators, ...columnComparators],
      },
    };
  }, {});
}
