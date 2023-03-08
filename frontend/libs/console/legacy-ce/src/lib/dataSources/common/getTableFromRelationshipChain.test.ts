import { getTableFromRelationshipChain } from '.';
import {
  expectedArrayResult,
  expectedObjectResult,
  startTable,
  allTables,
  objectAllTables,
  objectStartTable,
} from './getTableFromRelationshipChain.mocks';

describe('getTableFromRelationshipChain', () => {
  it('should find the referenced table based on array relationships using "_c"(list comparison) operators', () => {
    const result = getTableFromRelationshipChain(
      allTables,
      startTable,
      'classByClass.enum_relation._clte'
    );

    expect(result).toEqual(expectedArrayResult);
  });
});

describe('getTableFromRelationshipChain', () => {
  it('should find the referenced table based on object relationships using "_c"(list comparison) operators', () => {
    const result = getTableFromRelationshipChain(
      objectAllTables,
      objectStartTable,
      'object_relationship.id._ceq'
    );

    expect(result).toEqual(expectedObjectResult);
  });
});
