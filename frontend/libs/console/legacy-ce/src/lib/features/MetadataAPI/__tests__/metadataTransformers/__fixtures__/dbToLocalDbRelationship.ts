import type {
  ArrayRelationship,
  ObjectRelationship,
} from '../../../../../metadata/types';

export const dbToLocalDbRelationship = {
  objectRelationships: [
    {
      name: 'product_user',
      using: {
        foreign_key_constraint_on: 'fk_user_id',
      },
    },
  ] as ObjectRelationship[],
  arrayRelationships: [
    {
      name: 'user_product',
      using: {
        foreign_key_constraint_on: {
          column: 'fk_user_id',
          table: {
            schema: 'public',
            name: 'product',
          },
        },
      },
    },
  ] as ArrayRelationship[],
};

export const tableRelationships = [
  {
    from: {
      table: 'product',
      column: ['fk_user_id'],
    },
    to: {
      table: '"user"',
      column: ['id'],
    },
  },
];
