import { buildClientSchema } from 'graphql';
import {
  addImportedTypeSuffixIfNotPresent,
  generateAllTypeDefinitions,
  generateTypeDef,
  getAllTypeNames,
  getInnerType,
} from '../utils';
import { schema } from './mockSchema';

describe('addImportedTypeSuffixIfNotPresent', () => {
  it('should return type with suffix_ActionInputType ', async () => {
    const operations = await addImportedTypeSuffixIfNotPresent(
      'typeName',
      'input'
    );
    expect(operations).toEqual('typeName_ActionInputType');
  });
  it('should return type with suffix _ActionType ', async () => {
    const operations = await addImportedTypeSuffixIfNotPresent(
      'typeName',
      'type'
    );
    expect(operations).toEqual('typeName_ActionType');
  });
});

describe('getAllTypeNames', () => {
  it('should return all type names', async () => {
    const operations = await getAllTypeNames(buildClientSchema(schema), true);
    expect(operations).toMatchSnapshot();
  });
});

describe('generateAllTypeDefinitions', () => {
  it('should return all input type definitions', async () => {
    const operations = await generateAllTypeDefinitions(
      buildClientSchema(schema),
      ['Table', 'NewSchemaUserTable'],
      'input'
    );
    expect(operations).toMatchSnapshot();
  });
  it('should return all type definitions', async () => {
    const operations = await generateAllTypeDefinitions(
      buildClientSchema(schema),
      ['Table', 'NewSchemaUserTable'],
      'type'
    );
    expect(operations).toMatchSnapshot();
  });
});

describe('generateTypeDef', () => {
  it('should return input type definitions of a single typeName', async () => {
    const operations = await generateTypeDef(
      buildClientSchema(schema),
      'Table',
      'input'
    );
    expect(operations).toMatchSnapshot();
  });
});

describe('getInnerType', () => {
  it('should return type of a typeName', async () => {
    const operations = await getInnerType({
      description: null,
      deprecationReason: null,
      type: 'Int!',
      args: [],
      name: 'id',
      isDeprecated: false,
    } as any);
    expect(operations).toMatchSnapshot();
  });
});
