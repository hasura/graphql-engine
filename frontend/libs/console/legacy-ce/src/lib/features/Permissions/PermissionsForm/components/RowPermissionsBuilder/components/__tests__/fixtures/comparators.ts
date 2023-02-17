import { Comparators } from '../../types';
import {
  BooleanType,
  FloatType,
  IntType,
  StringType,
  stringType,
} from './graphql';

export const comparators: Comparators = {
  number_SQLite_comparison_exp: {
    operators: [
      {
        name: 'equals',
        operator: '_eq',
        type: IntType,
      },
      {
        name: '>',
        operator: '_gt',
        type: IntType,
      },
      {
        name: '>=',
        operator: '_gte',
        type: IntType,
      },
      {
        name: 'in',
        operator: '_in',
        type: IntType,
      },
      {
        name: 'is null',
        operator: '_is_null',
        type: IntType,
      },
      {
        name: '<',
        operator: '_lt',
        type: IntType,
      },
      {
        name: '<=',
        operator: '_lte',
        type: IntType,
      },
      {
        name: 'not equals',
        operator: '_neq',
        type: IntType,
      },
      {
        name: 'not in',
        operator: '_nin',
        type: IntType,
      },
    ],
  },
  string_SQLite_comparison_exp: {
    operators: [
      {
        name: 'equals',
        operator: '_eq',
        type: stringType,
      },
      {
        name: '>',
        operator: '_gt',
        type: stringType,
      },
      {
        name: '>=',
        operator: '_gte',
        type: stringType,
      },
      {
        name: 'in',
        operator: '_in',
        type: stringType,
      },
      {
        name: 'is null',
        operator: '_is_null',
        type: stringType,
      },
      {
        name: '<',
        operator: '_lt',
        type: stringType,
      },
      {
        name: '<=',
        operator: '_lte',
        type: stringType,
      },
      {
        name: 'not equals',
        operator: '_neq',
        type: stringType,
      },
      {
        name: 'not in',
        operator: '_nin',
        type: stringType,
      },
    ],
  },
  String_comparison_exp: {
    operators: [
      {
        name: 'equals',
        operator: '_eq',
        type: StringType,
      },
      {
        name: '>',
        operator: '_gt',
        type: StringType,
      },
      {
        name: '>=',
        operator: '_gte',
        type: StringType,
      },

      {
        name: 'like (case-insensitive)',
        operator: '_ilike',
        type: StringType,
      },
      {
        name: 'in',
        operator: '_in',
        type: StringType,
      },

      {
        name: '~*',
        operator: '_iregex',
        type: StringType,
      },
      {
        name: 'is null',
        operator: '_is_null',
        type: StringType,
      },

      {
        name: 'like',
        operator: '_like',
        type: StringType,
      },
      {
        name: '<',
        operator: '_lt',
        type: StringType,
      },
      {
        name: '<=',
        operator: '_lte',
        type: StringType,
      },
      {
        name: 'not equals',
        operator: '_neq',
        type: StringType,
      },

      {
        name: 'not like (case-insensitive)',
        operator: '_nilike',
        type: StringType,
      },
      {
        name: 'not in',
        operator: '_nin',
        type: StringType,
      },

      {
        name: '!~*',
        operator: '_niregex',
        type: StringType,
      },

      {
        name: 'not like',
        operator: '_nlike',
        type: StringType,
      },

      {
        name: '!~',
        operator: '_nregex',
        type: StringType,
      },

      {
        name: 'not similar',
        operator: '_nsimilar',
        type: StringType,
      },

      {
        name: '~',
        operator: '_regex',
        type: StringType,
      },

      {
        name: 'similar',
        operator: '_similar',
        type: StringType,
      },
    ],
  },
  Int_comparison_exp: {
    operators: [
      {
        name: 'equals',
        operator: '_eq',
        type: IntType,
      },
      {
        name: '>',
        operator: '_gt',
        type: IntType,
      },
      {
        name: '>=',
        operator: '_gte',
        type: IntType,
      },
      {
        name: 'in',
        operator: '_in',
        type: IntType,
      },
      {
        name: 'is null',
        operator: '_is_null',
        type: IntType,
      },
      {
        name: '<',
        operator: '_lt',
        type: IntType,
      },
      {
        name: '<=',
        operator: '_lte',
        type: IntType,
      },
      {
        name: 'not equals',
        operator: '_neq',
        type: IntType,
      },
      {
        name: 'not in',
        operator: '_nin',
        type: IntType,
      },
    ],
  },
  String_BigQuery_comparison_exp: {
    operators: [
      {
        name: 'equals',
        operator: '_eq',
        type: StringType,
      },
      {
        name: '>',
        operator: '_gt',
        type: StringType,
      },
      {
        name: '>=',
        operator: '_gte',
        type: StringType,
      },
      {
        name: 'in',
        operator: '_in',
        type: StringType,
      },
      {
        name: 'is null',
        operator: '_is_null',
        type: StringType,
      },
      {
        name: 'like',
        operator: '_like',
        type: StringType,
      },
      {
        name: '<',
        operator: '_lt',
        type: StringType,
      },
      {
        name: '<=',
        operator: '_lte',
        type: StringType,
      },
      {
        name: 'not equals',
        operator: '_neq',
        type: StringType,
      },
      {
        name: 'not in',
        operator: '_nin',
        type: StringType,
      },

      {
        name: 'not like',
        operator: '_nlike',
        type: StringType,
      },
    ],
  },
  Float_BigQuery_comparison_exp: {
    operators: [
      {
        name: 'equals',
        operator: '_eq',
        type: FloatType,
      },
      {
        name: '>',
        operator: '_gt',
        type: FloatType,
      },
      {
        name: '>=',
        operator: '_gte',
        type: FloatType,
      },
      {
        name: 'in',
        operator: '_in',
        type: FloatType,
      },
      {
        name: 'is null',
        operator: '_is_null',
        type: FloatType,
      },
      {
        name: '<',
        operator: '_lt',
        type: FloatType,
      },
      {
        name: '<=',
        operator: '_lte',
        type: FloatType,
      },
      {
        name: 'not equals',
        operator: '_neq',
        type: FloatType,
      },
      {
        name: 'not in',
        operator: '_nin',
        type: FloatType,
      },
    ],
  },
  Boolean_BigQuery_comparison_exp: {
    operators: [
      {
        name: 'equals',
        operator: '_eq',
        type: BooleanType,
      },
      {
        name: '>',
        operator: '_gt',
        type: BooleanType,
      },
      {
        name: '>=',
        operator: '_gte',
        type: BooleanType,
      },
      {
        name: 'in',
        operator: '_in',
        type: BooleanType,
      },
      {
        name: 'is null',
        operator: '_is_null',
        type: BooleanType,
      },
      {
        name: '<',
        operator: '_lt',
        type: BooleanType,
      },
      {
        name: '<=',
        operator: '_lte',
        type: BooleanType,
      },
      {
        name: 'not equals',
        operator: '_neq',
        type: BooleanType,
      },
      {
        name: 'not in',
        operator: '_nin',
        type: BooleanType,
      },
    ],
  },
  Int_BigQuery_comparison_exp: {
    operators: [
      {
        name: 'equals',
        operator: '_eq',
        type: IntType,
      },
      {
        name: '>',
        operator: '_gt',
        type: IntType,
      },
      {
        name: '>=',
        operator: '_gte',
        type: IntType,
      },
      {
        name: 'in',
        operator: '_in',
        type: IntType,
      },
      {
        name: 'is null',
        operator: '_is_null',
        type: IntType,
      },
      {
        name: '<',
        operator: '_lt',
        type: IntType,
      },
      {
        name: '<=',
        operator: '_lte',
        type: IntType,
      },
      {
        name: 'not equals',
        operator: '_neq',
        type: IntType,
      },
      {
        name: 'not in',
        operator: '_nin',
        type: IntType,
      },
    ],
  },
};
