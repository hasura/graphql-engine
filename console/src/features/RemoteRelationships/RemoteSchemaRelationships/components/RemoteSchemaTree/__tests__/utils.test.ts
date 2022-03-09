import { buildClientSchema } from 'graphql';
import hasura_schema from '../fixtures/hasura_schema.json';
import {
  getExpandedKeys,
  getCheckedKeys,
  buildServerRemoteFieldObject,
  parseServerRelationship,
  buildTree,
} from '../utils';
import {
  relationship_fields,
  remote_rel_definition,
  customer_columns,
} from '../fixtures/constants';

describe('Utils.ts', () => {
  it('buildServerRemoteFieldObject', () => {
    const remoteRelObj = buildServerRemoteFieldObject(relationship_fields);
    expect(remoteRelObj).toMatchSnapshot();
  });

  it('parseServerRelationship', () => {
    const relationshipFields = parseServerRelationship(remote_rel_definition);
    expect(relationshipFields).toMatchSnapshot();
  });

  it('buildTree', () => {
    const schema = buildClientSchema(hasura_schema as any);
    const tree = buildTree(schema, [], () => {}, customer_columns, [
      'query',
      'mutation',
      'subscription',
    ]);
    expect(tree).toMatchSnapshot();
  });

  it('getExpandedKeys', () => {
    const expandedKeys = [
      '__query',
      '__query.testUser_aggregate',
      '__query.testUser_aggregate.arguments.where',
      '__query.testUser_aggregate.arguments.where.id',
      '__query.testUser_aggregate.field.aggregate',
      '__query.testUser_aggregate.field.aggregate.field.count',
    ];
    expect(getExpandedKeys(relationship_fields)).toEqual(
      expect.arrayContaining(expandedKeys)
    );
  });

  it('gets checked keys', () => {
    const checkedKeys = [
      '__query.testUser_aggregate.arguments.where.id._eq',
      '__query.testUser_aggregate.field.aggregate.field.count.arguments.distinct',
    ];
    expect(getCheckedKeys(relationship_fields)).toEqual(
      expect.arrayContaining(checkedKeys)
    );
  });
});
