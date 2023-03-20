import { Comparators } from '../../types';
import {
  BooleanType,
  FloatType,
  IntType,
  StringType,
  stringType,
} from './graphql';

export const comparators: Comparators = {
  number_SQLite: {
    operators: [
      {
        name: '_eq',
        graphqlType: IntType,
        type: 'comparision',
      },
      {
        name: '_gt',
        graphqlType: IntType,
        type: 'comparision',
      },
      {
        name: '_gte',
        graphqlType: IntType,
        type: 'comparision',
      },
      {
        name: '_in',
        graphqlType: IntType,
        type: 'comparision',
      },
      {
        name: '_is_null',
        graphqlType: IntType,
        type: 'comparision',
      },
      {
        name: '_lt',
        graphqlType: IntType,
        type: 'comparision',
      },
      {
        name: '_lte',
        graphqlType: IntType,
        type: 'comparision',
      },
      {
        name: '_neq',
        graphqlType: IntType,
        type: 'comparision',
      },
      {
        name: '_nin',
        graphqlType: IntType,
        type: 'comparision',
      },
      {
        name: '_ceq',
        graphqlType: IntType,
        type: 'comparision',
      },
    ],
  },
  string_SQLite: {
    operators: [
      {
        name: '_eq',
        graphqlType: stringType,
        type: 'comparision',
      },
      {
        name: '_gt',
        graphqlType: stringType,
        type: 'comparision',
      },
      {
        name: '_gte',
        graphqlType: stringType,
        type: 'comparision',
      },
      {
        name: '_in',
        graphqlType: stringType,
        type: 'comparision',
      },
      {
        name: '_is_null',
        graphqlType: stringType,
        type: 'comparision',
      },
      {
        name: '_lt',
        graphqlType: stringType,
        type: 'comparision',
      },
      {
        name: '_lte',
        graphqlType: stringType,
        type: 'comparision',
      },
      {
        name: '_neq',
        graphqlType: stringType,
        type: 'comparision',
      },
      {
        name: '_nin',
        graphqlType: stringType,
        type: 'comparision',
      },
      {
        name: '_ceq',
        graphqlType: IntType,
        type: 'comparision',
      },
    ],
  },
  String: {
    operators: [
      {
        name: '_eq',
        graphqlType: StringType,
        type: 'comparision',
      },
      {
        name: '_gt',
        graphqlType: StringType,
        type: 'comparision',
      },
      {
        name: '_gte',
        graphqlType: StringType,
        type: 'comparision',
      },

      {
        name: '_ilike',
        graphqlType: StringType,
        type: 'comparision',
      },
      {
        name: '_in',
        graphqlType: StringType,
        type: 'comparision',
      },

      {
        name: '_iregex',
        graphqlType: StringType,
        type: 'comparision',
      },
      {
        name: '_is_null',
        graphqlType: StringType,
        type: 'comparision',
      },

      {
        name: '_like',
        graphqlType: StringType,
        type: 'comparision',
      },
      {
        name: '_lt',
        graphqlType: StringType,
        type: 'comparision',
      },
      {
        name: '_lte',
        graphqlType: StringType,
        type: 'comparision',
      },
      {
        name: '_neq',
        graphqlType: StringType,
        type: 'comparision',
      },

      {
        name: '_nilike',
        graphqlType: StringType,
        type: 'comparision',
      },
      {
        name: '_nin',
        graphqlType: StringType,
        type: 'comparision',
      },
      {
        name: '_ceq',
        graphqlType: IntType,
        type: 'comparision',
      },
      {
        name: '_niregex',
        graphqlType: StringType,
        type: 'comparision',
      },

      {
        name: '_nlike',
        graphqlType: StringType,
        type: 'comparision',
      },

      {
        name: '_nregex',
        graphqlType: StringType,
        type: 'comparision',
      },

      {
        name: '_nsimilar',
        graphqlType: StringType,
        type: 'comparision',
      },

      {
        name: '_regex',
        graphqlType: StringType,
        type: 'comparision',
      },

      {
        name: '_similar',
        graphqlType: StringType,
        type: 'comparision',
      },
    ],
  },
  Int: {
    operators: [
      {
        name: '_eq',
        graphqlType: IntType,
        type: 'comparision',
      },
      {
        name: '_gt',
        graphqlType: IntType,
        type: 'comparision',
      },
      {
        name: '_gte',
        graphqlType: IntType,
        type: 'comparision',
      },
      {
        name: '_in',
        graphqlType: IntType,
        type: 'comparision',
      },
      {
        name: '_is_null',
        graphqlType: IntType,
        type: 'comparision',
      },
      {
        name: '_lt',
        graphqlType: IntType,
        type: 'comparision',
      },
      {
        name: '_lte',
        graphqlType: IntType,
        type: 'comparision',
      },
      {
        name: '_neq',
        graphqlType: IntType,
        type: 'comparision',
      },
      {
        name: '_nin',
        graphqlType: IntType,
        type: 'comparision',
      },
      {
        name: '_ceq',
        graphqlType: IntType,
        type: 'comparision',
      },
    ],
  },
  String_BigQuery: {
    operators: [
      {
        name: '_eq',
        graphqlType: StringType,
        type: 'comparision',
      },
      {
        name: '_gt',
        graphqlType: StringType,
        type: 'comparision',
      },
      {
        name: '_gte',
        graphqlType: StringType,
        type: 'comparision',
      },
      {
        name: '_in',
        graphqlType: StringType,
        type: 'comparision',
      },
      {
        name: '_is_null',
        graphqlType: StringType,
        type: 'comparision',
      },
      {
        name: '_like',
        graphqlType: StringType,
        type: 'comparision',
      },
      {
        name: '_lt',
        graphqlType: StringType,
        type: 'comparision',
      },
      {
        name: '_lte',
        graphqlType: StringType,
        type: 'comparision',
      },
      {
        name: '_neq',
        graphqlType: StringType,
        type: 'comparision',
      },
      {
        name: '_nin',
        graphqlType: StringType,
        type: 'comparision',
      },
      {
        name: '_ceq',
        graphqlType: IntType,
        type: 'comparision',
      },

      {
        name: '_nlike',
        graphqlType: StringType,
        type: 'comparision',
      },
    ],
  },
  Float_BigQuery: {
    operators: [
      {
        name: '_eq',
        graphqlType: FloatType,
        type: 'comparision',
      },
      {
        name: '_gt',
        graphqlType: FloatType,
        type: 'comparision',
      },
      {
        name: '_gte',
        graphqlType: FloatType,
        type: 'comparision',
      },
      {
        name: '_in',
        graphqlType: FloatType,
        type: 'comparision',
      },
      {
        name: '_is_null',
        graphqlType: FloatType,
        type: 'comparision',
      },
      {
        name: '_lt',
        graphqlType: FloatType,
        type: 'comparision',
      },
      {
        name: '_lte',
        graphqlType: FloatType,
        type: 'comparision',
      },
      {
        name: '_neq',
        graphqlType: FloatType,
        type: 'comparision',
      },
      {
        name: '_nin',
        graphqlType: FloatType,
        type: 'comparision',
      },
      {
        name: '_ceq',
        graphqlType: IntType,
        type: 'comparision',
      },
    ],
  },
  Boolean_BigQuery: {
    operators: [
      {
        name: '_eq',
        graphqlType: BooleanType,
        type: 'comparision',
      },
      {
        name: '_gt',
        graphqlType: BooleanType,
        type: 'comparision',
      },
      {
        name: '_gte',
        graphqlType: BooleanType,
        type: 'comparision',
      },
      {
        name: '_in',
        graphqlType: BooleanType,
        type: 'comparision',
      },
      {
        name: '_is_null',
        graphqlType: BooleanType,
        type: 'comparision',
      },
      {
        name: '_lt',
        graphqlType: BooleanType,
        type: 'comparision',
      },
      {
        name: '_lte',
        graphqlType: BooleanType,
        type: 'comparision',
      },
      {
        name: '_neq',
        graphqlType: BooleanType,
        type: 'comparision',
      },
      {
        name: '_nin',
        graphqlType: BooleanType,
        type: 'comparision',
      },
      {
        name: '_ceq',
        graphqlType: IntType,
        type: 'comparision',
      },
    ],
  },
  Int_BigQuery: {
    operators: [
      {
        name: '_eq',
        graphqlType: IntType,
        type: 'comparision',
      },
      {
        name: '_gt',
        graphqlType: IntType,
        type: 'comparision',
      },
      {
        name: '_gte',
        graphqlType: IntType,
        type: 'comparision',
      },
      {
        name: '_in',
        graphqlType: IntType,
        type: 'comparision',
      },
      {
        name: '_is_null',
        graphqlType: IntType,
        type: 'comparision',
      },
      {
        name: '_lt',
        graphqlType: IntType,
        type: 'comparision',
      },
      {
        name: '_lte',
        graphqlType: IntType,
        type: 'comparision',
      },
      {
        name: '_neq',
        graphqlType: IntType,
        type: 'comparision',
      },
      {
        name: '_nin',
        graphqlType: IntType,
        type: 'comparision',
      },
      {
        name: '_ceq',
        graphqlType: IntType,
        type: 'comparision',
      },
    ],
  },
};
