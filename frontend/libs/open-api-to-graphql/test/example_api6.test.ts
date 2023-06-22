// Copyright IBM Corp. 2017,2018. All Rights Reserved.
// Node module: openapi-to-graphql
// This file is licensed under the MIT License.
// License text available at https://opensource.org/licenses/MIT

'use strict';

import { graphql, GraphQLSchema, parse, validate } from 'graphql';
import { afterAll, beforeAll, expect, test } from '@jest/globals';

import * as openAPIToGraphQL from '../src/index';
import { Options } from '../src/types/options';
import { startServer, stopServer } from './example_api6_server';

const oas = require('./fixtures/example_oas6.json');
const PORT = 3008;
// Update PORT for this test case:
oas.servers[0].variables.port.default = String(PORT);

let createdSchema: GraphQLSchema;

// Set up the schema first and run example API server
beforeAll(() => {
  return Promise.all([
    openAPIToGraphQL.createGraphQLSchema(oas).then(({ schema, report }) => {
      createdSchema = schema;
    }),
    startServer(PORT),
  ]);
});

// Shut down API server
afterAll(() => {
  return stopServer();
});

test('Option requestOptions should work with links', () => {
  // Verify the behavior of the link by itself
  const query = `{
    object {
      object2Link {
        data
      }
      withParameter: object2Link (specialheader: "extra data"){
        data
      }
    }
  }`;

  const promise = graphql({ schema: createdSchema, source: query }).then(
    result => {
      expect(result.data).toEqual({
        object: {
          object2Link: {
            data: 'object2',
          },
          withParameter: {
            data: "object2 with special header: 'extra data'",
          },
        },
      });
    }
  );

  const options: Options<any, any, any> = {
    requestOptions: {
      headers: {
        specialheader: 'requestOptions',
      },
    },
  };

  const query2 = `{
    object {
      object2Link {
        data
      }
    }
  }`;

  const promise2 = openAPIToGraphQL
    .createGraphQLSchema(oas, options)
    .then(({ schema }) => {
      const ast = parse(query2);
      const errors = validate(schema, ast);
      expect(errors).toEqual([]);
      return graphql({ schema, source: query2 }).then(result => {
        expect(result).toEqual({
          data: {
            object: {
              object2Link: {
                data: "object2 with special header: 'requestOptions'", // Data from requestOptions in a link
              },
            },
          },
        });
      });
    });

  return Promise.all([promise, promise2]);
});

// Simple scalar fields on the request body
test('Simple request body using application/x-www-form-urlencoded', () => {
  const query = `mutation {
    postFormUrlEncoded (petInput: {
      name: "Mittens",
      status: "healthy",
      weight: 6
    }) {
      name
      status
      weight
    }
  }`;

  return graphql({ schema: createdSchema, source: query }).then(result => {
    expect(result.data).toEqual({
      postFormUrlEncoded: {
        name: 'Mittens',
        status: 'healthy',
        weight: 6,
      },
    });
  });
});

/**
 * The field 'previousOwner' should be desanitized to 'previous_owner'
 *
 * Status is a required field so it is also included
 */
test('Request body using application/x-www-form-urlencoded and desanitization of field name', () => {
  const query = `mutation {
    postFormUrlEncoded (petInput: {
      previousOwner: "Martin",
      status: "healthy"
    }) {
      previousOwner
    }
  }`;

  return graphql({ schema: createdSchema, source: query }).then(result => {
    expect(result.data).toEqual({
      postFormUrlEncoded: {
        previousOwner: 'Martin',
      },
    });
  });
});

/**
 * The field 'history' is an object
 *
 * Status is a required field so it is also included
 */
test('Request body using application/x-www-form-urlencoded containing object', () => {
  const query = `mutation {
    postFormUrlEncoded (petInput: {
      history: {
        data: "Friendly"
      }
      status: "healthy"
    }) {
      history {
        data
      }
    }
  }`;

  return graphql({ schema: createdSchema, source: query }).then(result => {
    expect(result.data).toEqual({
      postFormUrlEncoded: {
        history: {
          data: 'Friendly',
        },
      },
    });
  });
});

test('Request body using application/x-www-form-urlencoded containing object with no properties', () => {
  const query = `mutation {
    postFormUrlEncoded (petInput: {
      history2: {
        data: "Friendly"
      }
      status: "healthy"
    }) {
      history2
    }
  }`;

  return graphql({ schema: createdSchema, source: query }).then(result => {
    expect(result.data).toEqual({
      postFormUrlEncoded: {
        history2: {
          data: 'Friendly',
        },
      },
    });
  });
});

/**
 * GET /cars/{id} should create a 'car' field
 *
 * Also the path parameter just contains the term 'id'
 */
test('inferResourceNameFromPath() field with simple plural form', () => {
  const query = `{
    car (id: "Super Speed")
  }`;

  return graphql({ schema: createdSchema, source: query }).then(result => {
    expect(result.data).toEqual({
      car: 'Car ID: Super Speed',
    });
  });
});

/**
 * GET /cacti/{cactusId} should create an 'cactus' field
 *
 * Also the path parameter is the combination of the singular form and 'id'
 */
test('inferResourceNameFromPath() field with irregular plural form', () => {
  const query = `{
    cactus (cactusId: "Spikey")
  }`;

  return graphql({ schema: createdSchema, source: query }).then(result => {
    expect(result.data).toEqual({
      cactus: 'Cactus ID: Spikey',
    });
  });
});

/**
 * GET /eateries/{eatery}/breads/{breadName}/dishes/{dishKey} should create an
 * 'eateryBreadDish' field
 *
 * The path parameters are the singular form, some combination with the term
 * 'name', and some combination with the term 'key'
 */
test('inferResourceNameFromPath() field with long path', () => {
  const query = `{
    eateryBreadDish(eatery: "Mike's", breadName: "challah", dishKey: "bread pudding")
  }`;

  return graphql({ schema: createdSchema, source: query }).then(result => {
    expect(result.data).toEqual({
      eateryBreadDish: "Parameters combined: Mike's challah bread pudding",
    });
  });
});

/**
 * '/nestedReferenceInParameter' contains a query parameter 'russianDoll' that
 * contains reference to a component schema.
 */
test('Nested reference in parameter schema', () => {
  const query = `{
    nestedReferenceInParameter(russianDoll: {
      name: "Gertrude",
      nestedDoll: {
        name: "Tatiana",
        nestedDoll: {
          name: "Lidia"
        }
      }
    })
  }`;

  return graphql({ schema: createdSchema, source: query }).then(result => {
    expect(result.data).toEqual({
      nestedReferenceInParameter: 'Gertrude, Tatiana, Lidia',
    });
  });
});

/**
 * 'POST inputUnion' has a request body that contains a oneOf. The request body
 * will be converted into an input object type while the oneOf will be turned
 * into a union type. However, according to the spec, input object types cannot
 * be composed of unions. As a fall back, this pattern should default to the
 * arbitrary JSON type instead.
 */
test('Input object types composed of union types should default to arbitrary JSON type', () => {
  const query = `{
    __type(name: "Mutation") {
      fields {
        name
        args {
          name
          type {
            name
          }
        }
      }
    }
  }`;

  return graphql({ schema: createdSchema, source: query }).then(result => {
    expect(
      result.data['__type']['fields'].find(
        field => field.name === 'postInputUnion'
      )
    ).toEqual({
      name: 'postInputUnion',
      args: [
        {
          name: 'inputUnionInput',
          type: {
            name: 'JSON',
          },
        },
      ],
    });
  });
});

/**
 * GET /strictGetOperation should not receive a Content-Type header
 */
test('Get operation should not receive Content-Type', () => {
  const query = `{
    strictGetOperation
  }`;

  return graphql({ schema: createdSchema, source: query }).then(result => {
    expect(result.data).toEqual({
      strictGetOperation: 'Perfect!',
    });
  });
});

/**
 * GET /noResponseSchema does not have a response schema
 */
test('Handle no response schema', () => {
  const query = `{
    noResponseSchema
  }`;

  return graphql({ schema: createdSchema, source: query }).then(result => {
    expect(result.data).toEqual({
      noResponseSchema: 'Hello world',
    });
  });
});

/**
 * GET /testLinkWithNonStringParam has a link object that has a non-string
 * parameter
 */
test('Handle no response schema', () => {
  const query = `{
    testLinkWithNonStringParam {
      hello
      return5
    }
  }`;

  return graphql({ schema: createdSchema, source: query }).then(result => {
    expect(result.data).toEqual({
      testLinkWithNonStringParam: {
        hello: 'world',
        return5: '5',
      },
    });
  });
});

/**
 * GET /testLinkwithNestedParam has a link object that has a nested
 * parameter
 */
test('Handle no response schema', () => {
  const query = `{
    testLinkwithNestedParam{
      nesting1 {
        nesting2
      }
      returnNestedNumber
    }
  }`;

  return graphql({ schema: createdSchema, source: query }).then(result => {
    expect(result.data).toEqual({
      testLinkwithNestedParam: {
        nesting1: {
          nesting2: 5,
        },
        returnNestedNumber: '5',
      },
    });
  });
});
