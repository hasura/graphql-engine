// Copyright IBM Corp. 2017,2018. All Rights Reserved.
// Node module: openapi-to-graphql
// This file is licensed under the MIT License.
// License text available at https://opensource.org/licenses/MIT

'use strict'

import { graphql, GraphQLSchema } from 'graphql'
import { afterAll, beforeAll, expect, test } from '@jest/globals'

import * as openAPIToGraphQL from '../src/index'

const oas = require('./fixtures/example_oas4.json')

let createdSchema: GraphQLSchema

// This test suite is used to verify the behavior of anyOf and oneOf handling

// Set up the schema
beforeAll(() => {
  return openAPIToGraphQL
    .createGraphQLSchema(oas)
    .then(({ schema, report }) => {
      createdSchema = schema
    })
})

const anyOfQuery = `{
  __schema {
    queryType {
      fields {
        name
        description
        type {
          name
          kind
          fields {
            name
            type {
              name
            }
          }
        }
      }
    }
  }
}`

const oneOfQuery = `{
  __schema {
    queryType {
      fields {
        name
        description
        type {
          name
          kind
          possibleTypes {
            name
            fields {
              type {
                name
              }
            }
          }
        }
      }
    }
  }
}`

/**
 * anyOf contains two member schemas
 *
 * Both member schemas contain the same field 'commonAttribute'
 *
 * Because they are the same, the created GraphQL object should only have one
 * 'commonAttribute' field
 */
test('Basic anyOf test using the same member schemas\n\nEquivalent to GET /anyOf', () => {
  return graphql({ schema: createdSchema, source: anyOfQuery }).then((result) => {
    expect(
      result.data['__schema']['queryType'].fields.find((field) => {
        return field.name === 'anyOf'
      })
    ).toEqual({
      name: 'anyOf',
      description:
        'Basic anyOf test using the same member schemas\n\nEquivalent to GET /anyOf',
      type: {
        name: 'AnyOf',
        kind: 'OBJECT',
        fields: [
          {
            name: 'commonAttribute',
            type: {
              name: 'String'
            }
          }
        ]
      }
    })
  })
})

/**
 * anyOf contains two member schemas
 *
 * One member schema contains a 'commonAttribute' field and the other
 * member schema contains a 'differentAttribute' field
 *
 * Because they are the different, the created GraphQL object should have both
 * fields
 */
test('Basic anyOf test with different member schemas\n\nEquivalent to GET /anyOf2', () => {
  return graphql({ schema: createdSchema, source: anyOfQuery }).then((result) => {
    expect(
      result.data['__schema']['queryType'].fields.find((field) => {
        return field.name === 'anyOf2'
      })
    ).toEqual({
      name: 'anyOf2',
      description:
        'Basic anyOf test with different member schemas\n\nEquivalent to GET /anyOf2',
      type: {
        name: 'AnyOf2',
        kind: 'OBJECT',
        fields: [
          {
            name: 'commonAttribute',
            type: {
              name: 'String'
            }
          },
          {
            name: 'differentAttribute',
            type: {
              name: 'String'
            }
          }
        ]
      }
    })
  })
})

/**
 * anyOf contains two member schemas
 *
 * Both member schemas contain the same complex nested field
 *
 * Because they are the same, the created GraphQL object should only have one
 * field
 */
test('anyOf test with the same nested member schemas\n\nEquivalent to GET /anyOf3', () => {
  return graphql({ schema: createdSchema, source: anyOfQuery }).then((result) => {
    expect(
      result.data['__schema']['queryType'].fields.find((field) => {
        return field.name === 'anyOf3'
      })
    ).toEqual({
      name: 'anyOf3',
      description:
        'anyOf test with the same nested member schemas\n\nEquivalent to GET /anyOf3',
      type: {
        name: 'AnyOf3',
        kind: 'OBJECT',
        fields: [
          {
            name: 'commonAttribute',
            type: {
              name: 'CommonAttribute'
            }
          }
        ]
      }
    })
  })
})

/**
 * anyOf contains two member schemas
 *
 * The member schemas contain complex nested fields that are different at the root
 * level.
 *
 * Because they are different at the root level, the created GraphQL object
 * should have two fields.
 */
test('anyOf test with different nested member schemas\n\nEquivalent to GET /anyOf4', () => {
  return graphql({ schema: createdSchema, source: anyOfQuery }).then((result) => {
    expect(
      result.data['__schema']['queryType'].fields.find((field) => {
        return field.name === 'anyOf4'
      })
    ).toEqual({
      name: 'anyOf4',
      description:
        'anyOf test with different nested member schemas\n\nEquivalent to GET /anyOf4',
      type: {
        name: 'AnyOf4',
        kind: 'OBJECT',
        fields: [
          {
            name: 'commonAttribute',
            type: {
              name: 'CommonAttribute'
            }
          },
          {
            name: 'differentAttribute',
            type: {
              name: 'DifferentAttribute'
            }
          }
        ]
      }
    })
  })
})

/**
 * anyOf contains two member schemas
 *
 * The member schemas contain complex nested fields that are same at the root
 * level but different at other levels.
 *
 * This leads to a conlict because the same field has different schemas. As a
 * result, the field will use the arbitrary JSON type.
 */
test('anyOf test with different nested member schemas, leading to conflict\n\nEquivalent to GET /anyOf5', () => {
  return graphql({ schema: createdSchema, source: anyOfQuery }).then((result) => {
    expect(
      result.data['__schema']['queryType'].fields.find((field) => {
        return field.name === 'anyOf5'
      })
    ).toEqual({
      name: 'anyOf5',
      description:
        'anyOf test with different nested member schemas, leading to conflict\n\nEquivalent to GET /anyOf5',
      type: {
        name: 'AnyOf5',
        kind: 'OBJECT',
        fields: [
          {
            name: 'commonAttribute',
            type: {
              name: 'JSON'
            }
          }
        ]
      }
    })
  })
})

/**
 * anyOf contains two member schemas
 *
 * The member schemas are of different types. One is an object type and the other
 * is an scalar type.
 *
 * This leads to a conlict. As a result, the field will use the arbitrary JSON
 * type.
 */
test('anyOf test with incompatible member schema types\n\nEquivalent to GET /anyOf6', () => {
  return graphql({ schema: createdSchema, source: anyOfQuery }).then((result) => {
    expect(
      result.data['__schema']['queryType'].fields.find((field) => {
        return field.name === 'anyOf6'
      })
    ).toEqual({
      name: 'anyOf6',
      description:
        'anyOf test with incompatible member schema types\n\nEquivalent to GET /anyOf6',
      type: {
        name: 'JSON',
        kind: 'SCALAR',
        fields: null
      }
    })
  })
})

/**
 * anyOf contains three member schemas
 *
 * Only one of the member schemas is an object type schema.
 *
 * The created type should be able to pick out the object type schema without
 * defaulting to the arbitrary JSON type.
 */
test('anyOf test with some extraneous member schemas\n\nEquivalent to GET /anyOf7', () => {
  return graphql({ schema: createdSchema, source: anyOfQuery }).then((result) => {
    expect(
      result.data['__schema']['queryType'].fields.find((field) => {
        return field.name === 'anyOf7'
      })
    ).toEqual({
      name: 'anyOf7',
      description:
        'anyOf test with some extraneous member schemas\n\nEquivalent to GET /anyOf7',
      type: {
        name: 'AnyOf7',
        kind: 'OBJECT',
        fields: [
          {
            name: 'commonAttribute',
            type: {
              name: 'String'
            }
          }
        ]
      }
    })
  })
})

/**
 * anyOf contains three member schemas
 *
 * Base schema has no target GraphQL type. One member schema has an integer
 * target type and the other two have no target types. Therefore, use integer
 * type.
 */
test('anyOf test with no object type member schemas\n\nEquivalent to GET /anyOf8', () => {
  return graphql({ schema: createdSchema, source: anyOfQuery }).then((result) => {
    expect(
      result.data['__schema']['queryType'].fields.find((field) => {
        return field.name === 'anyOf8'
      })
    ).toEqual({
      name: 'anyOf8',
      description:
        'anyOf test with no object type member schemas\n\nEquivalent to GET /anyOf8',
      type: {
        name: 'Int',
        kind: 'SCALAR',
        fields: null
      }
    })
  })
})

/**
 * anyOf contains three member schemas
 *
 * None of the member schemas are object type schemas but because there is an
 * external type provided in the root schema, it can utilize the proper typing.
 */
test('anyOf test with extraneous member schemas with external type\n\nEquivalent to GET /anyOf9', () => {
  return graphql({ schema: createdSchema, source: anyOfQuery }).then((result) => {
    expect(
      result.data['__schema']['queryType'].fields.find((field) => {
        return field.name === 'anyOf9'
      })
    ).toEqual({
      name: 'anyOf9',
      description:
        'anyOf test with extraneous member schemas with external type\n\nEquivalent to GET /anyOf9',
      type: {
        name: 'Int',
        kind: 'SCALAR',
        fields: null
      }
    })
  })
})

/**
 * anyOf contains two member schemas and allOf contains an additional one
 *
 * None of the schemas have conflicts so all three should be utilized
 */
test('Basic anyOf test with allOf\n\nEquivalent to GET /anyOf10', () => {
  return graphql({ schema: createdSchema, source: anyOfQuery }).then((result) => {
    expect(
      result.data['__schema']['queryType'].fields.find((field) => {
        return field.name === 'anyOf10'
      })
    ).toEqual({
      name: 'anyOf10',
      description: 'Basic anyOf test with allOf\n\nEquivalent to GET /anyOf10',
      type: {
        name: 'AnyOf10',
        kind: 'OBJECT',
        fields: [
          {
            name: 'anotherAttribute',
            type: {
              name: 'String'
            }
          },
          {
            name: 'commonAttribute',
            type: {
              name: 'String'
            }
          },
          {
            name: 'differentAttribute',
            type: {
              name: 'String'
            }
          }
        ]
      }
    })
  })
})

/**
 * anyOf contains two member schemas and allOf contains an additional one that
 * is nested in another anyOf
 *
 * Resolving the allOf should correctly collapse all of the (nested) anyOfs
 * and allow all three schemas to be utilized
 */
test('anyOf test with allOf, requiring anyOf collapse\n\nEquivalent to GET /anyOf11', () => {
  return graphql({ schema: createdSchema, source: anyOfQuery }).then((result) => {
    expect(
      result.data['__schema']['queryType'].fields.find((field) => {
        return field.name === 'anyOf11'
      })
    ).toEqual({
      name: 'anyOf11',
      description:
        'anyOf test with allOf, requiring anyOf collapse\n\nEquivalent to GET /anyOf11',
      type: {
        name: 'AnyOf11',
        kind: 'OBJECT',
        fields: [
          {
            name: 'anotherAttribute',
            type: {
              name: 'String'
            }
          },
          {
            name: 'commonAttribute',
            type: {
              name: 'String'
            }
          },
          {
            name: 'differentAttribute',
            type: {
              name: 'String'
            }
          }
        ]
      }
    })
  })
})

/**
 * oneOf contains two member schemas
 *
 * Because the schemas are different object types, the created GraphQL union
 * type has two differnet member types.
 */
test('Basic oneOf test\n\nEquivalent to GET /oneOf', () => {
  return graphql({ schema: createdSchema, source: oneOfQuery }).then((result) => {
    expect(
      result.data['__schema']['queryType'].fields.find((field) => {
        return field.name === 'oneOf'
      })
    ).toEqual({
      name: 'oneOf',
      description: 'Basic oneOf test\n\nEquivalent to GET /oneOf',
      type: {
        name: 'OneOf',
        kind: 'UNION',
        possibleTypes: [
          {
            name: 'CommonAttributeObject',
            fields: [
              {
                type: {
                  name: 'String'
                }
              }
            ]
          },
          {
            name: 'DifferentAttributeObject',
            fields: [
              {
                type: {
                  name: 'String'
                }
              }
            ]
          }
        ]
      }
    })
  })
})

/**
 * oneOf contains two member schemas
 *
 * Because one of the member schemas is not an object type, then default to
 * the arbitrary JSON type.
 */
test('oneOf test with non-object type member schema\n\nEquivalent to GET /oneOf2', () => {
  return graphql({ schema: createdSchema, source: oneOfQuery }).then((result) => {
    expect(
      result.data['__schema']['queryType'].fields.find((field) => {
        return field.name === 'oneOf2'
      })
    ).toEqual({
      name: 'oneOf2',
      description:
        'oneOf test with non-object type member schema\n\nEquivalent to GET /oneOf2',
      type: {
        name: 'JSON',
        kind: 'SCALAR',
        possibleTypes: null
      }
    })
  })
})

/**
 * oneOf contains two member schemas
 *
 * None of the member schemas are object types, therefore default to
 * the arbitrary JSON type.
 */
test('oneOf test with no object type member schemas\n\nEquivalent to GET /oneOf3', () => {
  return graphql({ schema: createdSchema, source: oneOfQuery }).then((result) => {
    expect(
      result.data['__schema']['queryType'].fields.find((field) => {
        return field.name === 'oneOf3'
      })
    ).toEqual({
      name: 'oneOf3',
      description:
        'oneOf test with no object type member schemas\n\nEquivalent to GET /oneOf3',
      type: {
        name: 'JSON',
        kind: 'SCALAR',
        possibleTypes: null
      }
    })
  })
})

/**
 * oneOf contains two member schemas
 *
 * The member schemas contain extranous data but because the root schema contains a
 * type, it is able to utilize the proper type.
 */
test('oneOf test with extraneous member schemas\n\nEquivalent to GET /oneOf4', () => {
  return graphql({ schema: createdSchema, source: oneOfQuery }).then((result) => {
    expect(
      result.data['__schema']['queryType'].fields.find((field) => {
        return field.name === 'oneOf4'
      })
    ).toEqual({
      name: 'oneOf4',
      description:
        'oneOf test with extraneous member schemas\n\nEquivalent to GET /oneOf4',
      type: {
        name: 'Int',
        kind: 'SCALAR',
        possibleTypes: null
      }
    })
  })
})

/**
 * oneOf contains two member schemas and an allOf
 *
 * Only schemas within the oneOf should be utilized
 *
 * TODO: verify this behavior and also create a test with additional root properties
 */
test('Basic oneOf test with allOf\n\nEquivalent to GET /oneOf5', () => {
  return graphql({ schema: createdSchema, source: oneOfQuery }).then((result) => {
    expect(
      result.data['__schema']['queryType'].fields.find((field) => {
        return field.name === 'oneOf5'
      })
    ).toEqual({
      name: 'oneOf5',
      description: 'Basic oneOf test with allOf\n\nEquivalent to GET /oneOf5',
      type: {
        name: 'OneOf5',
        kind: 'UNION',
        possibleTypes: [
          {
            name: 'CommonAttributeObject',
            fields: [
              {
                type: {
                  name: 'String'
                }
              }
            ]
          },
          {
            name: 'DifferentAttributeObject',
            fields: [
              {
                type: {
                  name: 'String'
                }
              }
            ]
          }
        ]
      }
    })
  })
})

/**
 * oneOf contains two member schemas and allOf contains an additional one that
 * is nested in another oneOf
 *
 * Resolving the allOf should correctly collapse all of the (nested) oneOfs
 * and allow all three schemas to be utilized
 */
test('oneOf test with allOf, requiring oneOf collapse\n\nEquivalent to GET /oneOf6', () => {
  return graphql({ schema: createdSchema, source: oneOfQuery }).then((result) => {
    expect(
      result.data['__schema']['queryType'].fields.find((field) => {
        return field.name === 'oneOf6'
      })
    ).toEqual({
      name: 'oneOf6',
      description:
        'oneOf test with allOf, requiring oneOf collapse\n\nEquivalent to GET /oneOf6',
      type: {
        name: 'OneOf6',
        kind: 'UNION',
        possibleTypes: [
          {
            name: 'CommonAttributeObject',
            fields: [
              {
                type: {
                  name: 'String'
                }
              }
            ]
          },
          {
            name: 'DifferentAttributeObject',
            fields: [
              {
                type: {
                  name: 'String'
                }
              }
            ]
          },
          {
            name: 'AnotherAttributeObject',
            fields: [
              {
                type: {
                  name: 'String'
                }
              }
            ]
          }
        ]
      }
    })
  })
})

/**
 * oneOf contains two member schemas, each with allOf
 *
 * oneOf also contains a link object
 *
 * Resolving the oneOf and allOfs should correctly create a union of two object
 * types, each object type with a link field from the oneOf schema
 */
 test('oneOf test with allOfs, requiring oneOf collapse\n\nEquivalent to GET /OneOfWithAllOfsAndLink', () => {
  return graphql({ schema: createdSchema, source: oneOfQuery }).then((result) => {
    expect(
      result.data['__schema']['queryType'].fields.find((field) => {
        return field.name === 'oneOfWithAllOfsAndLink'
      })
    ).toEqual({
      "name": "oneOfWithAllOfsAndLink",
      "description": "Equivalent to GET /oneOfWithAllOfsAndLink",
      "type": {
        "name": "OneOfWithAllOfsAndLink",
        "kind": "UNION",
        "possibleTypes": [
          {
            "name": "One",
            "fields": [
              {
                "type": {
                  "name": "String"
                }
              },
              {
                "type": {
                  "name": "String"
                }
              },
              {
                "type": {
                  "name": "String"
                }
              }
            ]
          },
          {
            "name": "Two",
            "fields": [
              {
                "type": {
                  "name": "String"
                }
              },
              {
                "type": {
                  "name": "String"
                }
              },
              {
                "type": {
                  "name": "String"
                }
              }
            ]
          }
        ]
      }
    })
  })
})
