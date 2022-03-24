import { getRemoteRelationType, getRemoteFieldPath } from '../utils';

describe('getRemoteRelationType', () => {
  it('should work with current remote relationship definition', () => {
    const res = getRemoteRelationType({
      definition: {
        remote_field: {
          country: {
            field: {
              continent: {
                field: {
                  countries: {
                    field: {
                      states: {
                        arguments: {},
                      },
                    },
                    arguments: {},
                  },
                },
                arguments: {},
              },
            },
            arguments: {},
          },
        },
        hasura_fields: [],
        remote_schema: 'countries',
      },
      source_name: 'default',
      name: 'states',
    });
    expect(res).toMatchSnapshot();
  });
  it('should work with new remote schema relationship definition', () => {
    const res = getRemoteRelationType({
      definition: {
        to_remote_schema: {
          remote_schema: 'countries_rs',
          remote_field: { continents: { arguments: {} } },
        },
      },
      source_name: 'default',
      name: 'continents',
    });
    expect(res).toMatchSnapshot();
  });
  it('should work with new remote source relationship definition', () => {
    const res = getRemoteRelationType({
      definition: {
        to_source: {
          relationship_type: 'object',
          source: 'chinook',
          table: 'Album',
          field_mapping: {
            artistId: 'id',
          },
        },
      },
      name: 'name_of_the_remote_relationship_1',
    });
    expect(res).toMatchSnapshot();
  });
});
describe('getRemoteFieldPath', () => {
  it('should work with simple remote field', () => {
    const res = getRemoteFieldPath({ continents: { arguments: {} } });
    expect(res).toMatchSnapshot();
  });
  it('should work with long remote field', () => {
    const res = getRemoteFieldPath({
      country: {
        field: {
          continent: {
            field: {
              countries: {
                field: {
                  states: {
                    arguments: {},
                  },
                },
                arguments: {},
              },
            },
            arguments: {},
          },
        },
        arguments: {},
      },
    });
    expect(res).toMatchSnapshot();
  });
});
