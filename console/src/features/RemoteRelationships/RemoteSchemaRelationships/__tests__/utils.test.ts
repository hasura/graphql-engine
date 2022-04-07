import { generateLhsFields, getFieldTypesFromType } from '../utils';

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
