import { buildClientSchema } from 'graphql';
import { schema } from '../__mocks__/schema';
import {
  generateLhsFields,
  getFieldTypesFromType,
  getTypesFromIntrospection,
} from '../utils';

describe('generateLhsFields', () => {
  it('with 1 source field in the resultset', () => {
    const lhsFields = generateLhsFields({
      arguments: {
        code: '$weight',
      },
    });
    expect(lhsFields).toEqual(['weight']);
  });
  it('with multiple source field in the resultset', () => {
    const lhsFields = generateLhsFields({
      arguments: {
        filter: {
          code: {
            eq: '$minimum',
            ne: '$minimum',
            nin: '$maximum',
          },
        },
      },
    });
    expect(lhsFields).toEqual(['minimum', 'maximum']);
  });
  it('with no source field in the resultset', () => {
    const lhsFields = generateLhsFields({
      arguments: {
        code: 'test',
      },
    });
    expect(lhsFields).toEqual([]);
  });
  it('with empty resultset', () => {
    const lhsFields = generateLhsFields({});
    expect(lhsFields).toEqual([]);
  });
});
describe('getFieldTypesFromType', () => {
  const remoteSchemaTypes = [
    {
      typeName: 'Query',
      fields: ['query', 'pokemons', 'pokemon'],
    },
    {
      typeName: 'Pokemon',
      fields: [
        'id',
        'number',
        'name',
        'weight',
        'height',
        'classification',
        'types',
        'resistant',
        'attacks',
        'weaknesses',
        'fleeRate',
        'maxCP',
        'evolutions',
        'evolutionRequirements',
        'maxHP',
        'image',
      ],
    },
    {
      typeName: 'PokemonDimension',
      fields: ['minimum', 'maximum'],
    },
    {
      typeName: 'PokemonAttack',
      fields: ['fast', 'special'],
    },
    {
      typeName: 'Attack',
      fields: ['name', 'type', 'damage'],
    },
    {
      typeName: 'PokemonEvolutionRequirement',
      fields: ['amount', 'name'],
    },
  ];
  it('with valid schemaTypes and sourceType', () => {
    const lhsFields = getFieldTypesFromType(remoteSchemaTypes, 'PokemonAttack');
    expect(lhsFields).toEqual(['fast', 'special']);
  });
  it('with valid schemaTypes and sourceType 2', () => {
    const lhsFields = getFieldTypesFromType(remoteSchemaTypes, 'Pokemon');
    expect(lhsFields).toEqual([
      'id',
      'number',
      'name',
      'weight',
      'height',
      'classification',
      'types',
      'resistant',
      'attacks',
      'weaknesses',
      'fleeRate',
      'maxCP',
      'evolutions',
      'evolutionRequirements',
      'maxHP',
      'image',
    ]);
  });
  it('with invalid schemaTypes and sourceType', () => {
    const lhsFields = getFieldTypesFromType(remoteSchemaTypes, 'Pokemonss');
    expect(lhsFields).toEqual([]);
  });
});

describe(' getTypesFromIntrospection', () => {
  it('filter out all types other then obejct type', () => {
    const graphQLSchema = buildClientSchema(schema.data as any);
    const filterTypes = getTypesFromIntrospection(graphQLSchema);
    // expect to see Quey and Countary because these are object type
    expect(filterTypes[0]?.typeName).toEqual('Query');
    expect(filterTypes[1]?.typeName).toEqual('Country');
    // beacuse ID, Boolean is Scalar type
    const filterScalarType = filterTypes.find(
      obj => obj.typeName === 'ID',
      'Boolean'
    );
    expect(filterScalarType).toBe(undefined);
    // because __TypeKind is of ENUM type
    const filterEnumType = filterTypes.find(
      obj => obj.typeName === '__TypeKind'
    );
    expect(filterEnumType).toBe(undefined);
    // snapshot only contains object types
    expect(filterTypes).toMatchInlineSnapshot(`
      [
        {
          "fields": [
            "_entities",
            "_service",
            "countries",
            "country",
            "continents",
            "continent",
            "languages",
            "language",
          ],
          "typeName": "Query",
        },
        {
          "fields": [
            "code",
            "name",
            "native",
            "phone",
            "continent",
            "capital",
            "currency",
            "languages",
            "emoji",
            "emojiU",
            "states",
          ],
          "typeName": "Country",
        },
        {
          "fields": [
            "code",
            "name",
            "countries",
          ],
          "typeName": "Continent",
        },
        {
          "fields": [
            "code",
            "name",
            "native",
            "rtl",
          ],
          "typeName": "Language",
        },
        {
          "fields": [
            "code",
            "name",
            "country",
          ],
          "typeName": "State",
        },
        {
          "fields": [
            "sdl",
          ],
          "typeName": "_Service",
        },
      ]
    `);
  });
});
