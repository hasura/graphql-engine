import { getEndpoints } from './Endpoints';
import globals from './Globals';

describe('Endpoints > luxDataGraphQL', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });
  afterAll(() => {
    jest.clearAllMocks();
  });

  it('when https domain is set, uses an HTTPS protocol', () => {
    // expect right values without SSL
    Object.defineProperty(window, 'location', {
      value: {
        protocol: 'https:',
      },
      writable: true,
    });

    const endpoints = getEndpoints({
      ...globals,
      luxDataHost: 'data.hasura.io',
    });
    expect(endpoints.luxDataGraphql).toEqual(
      'https://data.hasura.io/v1/graphql'
    );
    expect(endpoints.luxDataGraphqlWs).toEqual(
      'wss://data.hasura.io/v1/graphql'
    );
  });

  it('when http domain is set, uses an HTTP protocol', () => {
    // expect right values without SSL
    Object.defineProperty(window, 'location', {
      value: {
        protocol: 'http:',
      },
    });

    const endpoints = getEndpoints({
      ...globals,
      luxDataHost: 'data.lux-dev.hasura.me',
    });
    expect(endpoints.luxDataGraphql).toEqual(
      'http://data.lux-dev.hasura.me/v1/graphql'
    );
    expect(endpoints.luxDataGraphqlWs).toEqual(
      'ws://data.lux-dev.hasura.me/v1/graphql'
    );
  });
});
