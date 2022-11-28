import { RelationshipFields } from '../types';

export const customer_columns = [
  'id',
  'firstName',
  'lastName',
  'age',
  'countryCode',
  'country',
];

export const remote_rel_definition: any = {
  definition: {
    remote_field: {
      testUser_aggregate: {
        field: {
          aggregate: {
            field: {
              count: {
                arguments: {
                  columns: '$firstName',
                },
              },
            },
            arguments: {},
          },
        },
        arguments: {},
      },
    },
    hasura_fields: ['name'],
    remote_schema: 'hasura_cloud',
  },
  name: 'remoteSchema1',
};

export const relationship_fields: RelationshipFields[] = [
  {
    key: '__query',
    depth: 0,
    checkable: false,
    argValue: null,
    type: 'field',
  },
  {
    key: '__query.testUser_aggregate',
    depth: 1,
    checkable: false,
    argValue: null,
    type: 'field',
  },
  {
    key: '__query.testUser_aggregate.arguments.where',
    depth: 1,
    checkable: false,
    argValue: null,
    type: 'arg',
  },
  {
    key: '__query.testUser_aggregate.arguments.where.id',
    depth: 2,
    checkable: false,
    argValue: null,
    type: 'arg',
  },
  {
    key: '__query.testUser_aggregate.arguments.where.id._eq',
    depth: 3,
    checkable: true,
    argValue: {
      kind: 'field',
      value: 'id',
      type: 'String',
    },
    type: 'arg',
  },
  {
    key: '__query.testUser_aggregate.field.aggregate',
    depth: 2,
    checkable: false,
    argValue: null,
    type: 'field',
  },
  {
    key: '__query.testUser_aggregate.field.aggregate.field.count',
    depth: 3,
    checkable: false,
    argValue: null,
    type: 'field',
  },
  {
    key: '__query.testUser_aggregate.field.aggregate.field.count.arguments.distinct',
    depth: 3,
    checkable: true,
    argValue: {
      kind: 'field',
      value: 'id',
      type: 'String',
    },
    type: 'arg',
  },
];
