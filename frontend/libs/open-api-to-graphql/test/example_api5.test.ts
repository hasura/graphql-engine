// Copyright IBM Corp. 2017,2018. All Rights Reserved.
// Node module: openapi-to-graphql
// This file is licensed under the MIT License.
// License text available at https://opensource.org/licenses/MIT

'use strict';

import { graphql, GraphQLSchema } from 'graphql';
import { afterAll, beforeAll, expect, test } from '@jest/globals';

import * as openAPIToGraphQL from '../src/index';
import { startServer, stopServer } from './example_api5_server';

const oas = require('./fixtures/example_oas5.json');
const PORT = 3007;
// Update PORT for this test case:
oas.servers[0].variables.port.default = String(PORT);

// Testing the simpleNames option

let createdSchema: GraphQLSchema;

// Set up the schema first and run example API server
beforeAll(() => {
  return Promise.all([
    openAPIToGraphQL
      .createGraphQLSchema(oas, {
        simpleNames: true,
      })
      .then(({ schema, report }) => {
        createdSchema = schema;
      }),
    startServer(PORT),
  ]);
});

// Shut down API server
afterAll(() => {
  return stopServer();
});

/**
 * Because of the simpleNames option, 'o_d_d___n_a_m_e' will not be turned into
 * 'oDDNAME'.
 */
test('Basic simpleNames option test', () => {
  const query = `{
    o_d_d___n_a_m_e {
      data
    }
  }`;

  return graphql({ schema: createdSchema, source: query }).then(result => {
    expect(result).toEqual({
      data: {
        o_d_d___n_a_m_e: {
          data: 'odd name',
        },
      },
    });
  });
});

/**
 * 'w-e-i-r-d___n-a-m-e' contains GraphQL unsafe characters.
 *
 * Because of the simpleNames option, 'w-e-i-r-d___n-a-m-e' will be turned into
 * 'weird___name' and not 'wEIRDNAME'.
 */
test('Basic simpleNames option test with GraphQL unsafe values', () => {
  const query = `{
    weird___name {
      data
    }
  }`;

  return graphql({ schema: createdSchema, source: query }).then(result => {
    expect(result).toEqual({
      data: {
        weird___name: {
          data: 'weird name',
        },
      },
    });
  });
});

/**
 * 'w-e-i-r-d___n-a-m-e2' contains GraphQL unsafe characters.
 *
 * Because of the simpleNames option, 'w-e-i-r-d___n-a-m-e2' will be turned into
 * 'weird___name2' and not 'wEIRDNAME2'.
 */
test('Basic simpleNames option test with GraphQL unsafe values and a parameter', () => {
  const query = `{
    weird___name2 (funky___parameter: "Arnold") {
      data
    }
  }`;

  return graphql({ schema: createdSchema, source: query }).then(result => {
    expect(result).toEqual({
      data: {
        weird___name2: {
          data: 'weird name 2 param: Arnold',
        },
      },
    });
  });
});

/**
 * Because of the simpleNames option, 'w-e-i-r-d___n-a-m-e___l-i-n-k' will be
 * turned into 'weird___name___link' and not 'wEIRDNAMELINK'.
 */
test('Basic simpleNames option test with a link', () => {
  const query = `{
    o_d_d___n_a_m_e {
      weird___name___link {
        data
      }
    }
  }`;

  return graphql({ schema: createdSchema, source: query }).then(result => {
    expect(result).toEqual({
      data: {
        o_d_d___n_a_m_e: {
          weird___name___link: {
            data: 'weird name',
          },
        },
      },
    });
  });
});

/**
 * Because of the simpleNames option, 'w-e-i-r-d___n-a-m-e2___l-i-n-k' will be
 * turned into 'weird___name2___link' and not 'wEIRDNAME2LINK'.
 */
test('Basic simpleNames option test with a link that has parameters', () => {
  const query = `{
    o_d_d___n_a_m_e {
      weird___name2___link {
        data
      }
    }
  }`;

  return graphql({ schema: createdSchema, source: query }).then(result => {
    expect(result).toEqual({
      data: {
        o_d_d___n_a_m_e: {
          weird___name2___link: {
            data: 'weird name 2 param: Charles',
          },
        },
      },
    });
  });
});

/**
 * Because of the simpleNames option, 'w-e-i-r-d___n-a-m-e3___l-i-n-k' will be
 * turned into 'weird___name3___link3' and not 'wEIRDNAME3LINK'.
 */
test('Basic simpleNames option test with a link that has exposed parameters', () => {
  const query = `{
    o_d_d___n_a_m_e {
      weird___name3___link (funky___parameter: "Brittany") {
        data
      }
    }
  }`;

  return graphql({ schema: createdSchema, source: query }).then(result => {
    expect(result).toEqual({
      data: {
        o_d_d___n_a_m_e: {
          weird___name3___link: {
            data: 'weird name 3 param: Brittany',
          },
        },
      },
    });
  });
});

/**
 * Because of the simpleEnumValues option, 'a-m-b-e-r' will be sanitized to
 * ALL_CAPS 'A_M_B_E_R' when it is not used and sanitized to 'amber' (only
 * removing GraphQL illegal characters) when it is used
 */
test('Basic simpleEnumValues option test', () => {
  const query = `{
    getEnum {
      data
    }
  }`;

  const promise = graphql({ schema: createdSchema, source: query }).then(
    result => {
      expect(result).toEqual({
        data: {
          getEnum: {
            data: 'A_M_B_E_R',
          },
        },
      });
    }
  );

  const promise2 = openAPIToGraphQL
    .createGraphQLSchema(oas, {
      simpleEnumValues: true,
    })
    .then(({ schema, report }) => {
      return graphql({ schema, source: query }).then(result => {
        expect(result).toEqual({
          data: {
            getEnum: {
              data: 'amber',
            },
          },
        });
      });
    });

  return Promise.all([promise, promise2]);
});

/**
 * Regardless of simpleEnumValues, a GraphQL name cannot begin with a number,
 * therefore 3 will be sanitized to '_3'
 */
test('Basic simpleEnumValues option test on numerical enum', () => {
  const query = `{
    getNumericalEnum {
      data
    }
  }`;

  const promise = graphql({ schema: createdSchema, source: query }).then(
    result => {
      expect(result).toEqual({
        data: {
          getNumericalEnum: {
            data: '_3',
          },
        },
      });
    }
  );

  const promise2 = openAPIToGraphQL
    .createGraphQLSchema(oas, {
      simpleEnumValues: true,
    })
    .then(({ schema, report }) => {
      return graphql({ schema, source: query }).then(result => {
        expect(result).toEqual({
          data: {
            getNumericalEnum: {
              data: '_3',
            },
          },
        });
      });
    });

  return Promise.all([promise, promise2]);
});

/**
 * Regardless of simpleEnumValues, OtG will translate an object enum to an
 * arbitrary JSON type
 */
test('Basic simpleEnumValues option test on object enum', () => {
  const query = `{
    __type(name: "GetObjectEnum") {
      name
      kind
    } 
  }`;

  const promise = graphql({ schema: createdSchema, source: query }).then(
    result => {
      expect(result).toEqual({
        data: {
          __type: {
            name: 'GetObjectEnum',
            kind: 'OBJECT',
          },
        },
      });
    }
  );

  const promise2 = openAPIToGraphQL
    .createGraphQLSchema(oas, {
      simpleEnumValues: true,
    })
    .then(({ schema, report }) => {
      return graphql({ schema, source: query }).then(result => {
        expect(result).toEqual({
          data: {
            __type: {
              name: 'GetObjectEnum',
              kind: 'OBJECT',
            },
          },
        });
      });
    });

  return Promise.all([promise, promise2]);
});
