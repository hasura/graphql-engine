import { buildClientSchema, buildSchema, GraphQLSchema } from 'graphql';
import {
  getSchemaRoots,
  getTree,
  getTrimmedReturnType,
  getType,
  getRemoteSchemaFields,
  addPresetDefinition,
  generateSDL,
} from '../utils';
import schema1 from './fixtures/schema1.json';
import schema2 from './fixtures/schema2.json';
import { continentsPreset, userPreset } from './fixtures/roles.json';
import { args1, args2 } from './fixtures/args.json';

describe('Utils.ts', () => {
  let clientSchema1: GraphQLSchema;
  let clientSchema2: GraphQLSchema;
  beforeEach(async () => {
    clientSchema1 = buildClientSchema(schema1 as any);
    clientSchema2 = buildClientSchema(schema2 as any);
  });

  test('getTrimmedReturnType', () => {
    expect(getTrimmedReturnType(`[!User]`)).toBe(`User`);
    expect(getTrimmedReturnType(`[Book]`)).toBe(`Book`);
    expect(getTrimmedReturnType(`![!Book]`)).toBe(`Book`);
    expect(getTrimmedReturnType(`![!test]`)).toBe(`test`);
  });

  it('getTree', () => {
    const clientQueryTree1 = getTree(clientSchema1, null, 'QUERY');
    const clientMutnTree1 = getTree(clientSchema1, null, 'MUTATION');
    expect(clientQueryTree1.length).toBe(6);
    expect(clientMutnTree1.length).toBe(12);
    expect(clientQueryTree1).toMatchSnapshot();
    expect(clientMutnTree1).toMatchSnapshot();

    const clientQueryTree2 = getTree(clientSchema2, null, 'QUERY');
    const clientMutnTree2 = getTree(clientSchema2, null, 'MUTATION');
    expect(clientQueryTree2.length).toBe(6);
    expect(clientMutnTree2.length).toBe(0);
    expect(clientQueryTree2).toMatchSnapshot();
    expect(clientMutnTree2).toMatchSnapshot();
  });
  it('getSchemaRoots', () => {
    const schemaRoots = getSchemaRoots(clientSchema1);
    expect(schemaRoots.length).toBe(3);
    expect(schemaRoots).toMatchSnapshot();

    const schemaRoots2 = getSchemaRoots(clientSchema2);
    expect(schemaRoots2.length).toBe(1);
    expect(schemaRoots2).toMatchSnapshot();
  });
  it('getTypes', () => {
    const types = getType(clientSchema1, null);
    expect(types.length).toBe(82);
    expect(types).toMatchSnapshot();

    const types2 = getType(clientSchema2, null);
    expect(types2.length).toBe(14);
    expect(types2).toMatchSnapshot();
  });

  it('getSchemaRoots', () => {
    const roots = getSchemaRoots(clientSchema1);
    expect(roots.length).toBe(3);
    expect(roots).toMatchSnapshot();

    const roots2 = getSchemaRoots(clientSchema2);
    expect(roots2.length).toBe(1);
    expect(roots2).toMatchSnapshot();
  });

  it('getRemoteSchemaFields', () => {
    const def1 = addPresetDefinition(userPreset);
    const permissionsSchema1 = buildSchema(def1);
    const schemaFields1 = getRemoteSchemaFields(
      clientSchema1,
      permissionsSchema1
    );
    expect(schemaFields1).toMatchSnapshot();

    const def2 = addPresetDefinition(continentsPreset);
    const permissionsSchema2 = buildSchema(def2);
    const schemaFields2 = getRemoteSchemaFields(
      clientSchema2,
      permissionsSchema2
    );
    expect(schemaFields2).toMatchSnapshot();
  });
  it('generateSDL', () => {
    const def1 = addPresetDefinition(userPreset);
    const permissionsSchema1 = buildSchema(def1);
    const schemaFields1 = getRemoteSchemaFields(
      clientSchema1,
      permissionsSchema1
    );
    // w/o args
    const sdl1 = generateSDL(schemaFields1, {});
    expect(sdl1).toMatchSnapshot();

    // with presets
    const sdl1_args = generateSDL(schemaFields1, args1);
    expect(sdl1_args).toMatchSnapshot();

    const def2 = addPresetDefinition(continentsPreset);

    const permissionsSchema2 = buildSchema(def2);
    const schemaFields2 = getRemoteSchemaFields(
      clientSchema2,
      permissionsSchema2
    );

    // w/o args
    const sdl2 = generateSDL(schemaFields2, {});
    expect(sdl2).toMatchSnapshot();

    // with presets
    const sdl2_args = generateSDL(schemaFields2, args2);
    expect(sdl2_args).toMatchSnapshot();

    // w/o query root, should not have schema prefix
    const schemaFields3 = getType(clientSchema1, permissionsSchema1);
    const sdl3 = generateSDL(schemaFields3, {});
    expect(sdl3).toMatchSnapshot();
  });
});
