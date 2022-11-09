export const schema = {
  data: {
    __schema: {
      queryType: {
        name: 'Query',
      },
      mutationType: null,
      subscriptionType: null,
      types: [
        {
          kind: 'SCALAR',
          name: 'Boolean',
          description:
            'The `Boolean` scalar type represents `true` or `false`.',
          fields: null,
          inputFields: null,
          interfaces: null,
          enumValues: null,
          possibleTypes: null,
        },
        {
          kind: 'SCALAR',
          name: 'String',
          description:
            'The `String` scalar type represents textual data, represented as UTF-8 character sequences. The String type is most often used by GraphQL to represent free-form human-readable text.',
          fields: null,
          inputFields: null,
          interfaces: null,
          enumValues: null,
          possibleTypes: null,
        },
        {
          kind: 'OBJECT',
          name: 'Country',
          description: null,
          fields: [
            {
              name: 'code',
              description: null,
              args: [],
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'SCALAR',
                  name: 'ID',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'name',
              description: null,
              args: [],
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'SCALAR',
                  name: 'String',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'native',
              description: null,
              args: [],
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'SCALAR',
                  name: 'String',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'phone',
              description: null,
              args: [],
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'SCALAR',
                  name: 'String',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'continent',
              description: null,
              args: [],
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'OBJECT',
                  name: 'Continent',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'capital',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'currency',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'languages',
              description: null,
              args: [],
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'OBJECT',
                      name: 'Language',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'emoji',
              description: null,
              args: [],
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'SCALAR',
                  name: 'String',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'emojiU',
              description: null,
              args: [],
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'SCALAR',
                  name: 'String',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'states',
              description: null,
              args: [],
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'OBJECT',
                      name: 'State',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
          ],
          inputFields: null,
          interfaces: [],
          enumValues: null,
          possibleTypes: null,
        },
        {
          kind: 'SCALAR',
          name: 'ID',
          description:
            'The `ID` scalar type represents a unique identifier, often used to refetch an object or as key for a cache. The ID type appears in a JSON response as a String; however, it is not intended to be human-readable. When expected as an input type, any string (such as `"4"`) or integer (such as `4`) input value will be accepted as an ID.',
          fields: null,
          inputFields: null,
          interfaces: null,
          enumValues: null,
          possibleTypes: null,
        },
        {
          kind: 'OBJECT',
          name: 'Continent',
          description: null,
          fields: [
            {
              name: 'code',
              description: null,
              args: [],
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'SCALAR',
                  name: 'ID',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'name',
              description: null,
              args: [],
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'SCALAR',
                  name: 'String',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'countries',
              description: null,
              args: [],
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'OBJECT',
                      name: 'Country',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
          ],
          inputFields: null,
          interfaces: [],
          enumValues: null,
          possibleTypes: null,
        },
        {
          kind: 'OBJECT',
          name: 'Language',
          description: null,
          fields: [
            {
              name: 'code',
              description: null,
              args: [],
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'SCALAR',
                  name: 'ID',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'name',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'native',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'rtl',
              description: null,
              args: [],
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'SCALAR',
                  name: 'Boolean',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
          ],
          inputFields: null,
          interfaces: [],
          enumValues: null,
          possibleTypes: null,
        },
        {
          kind: 'OBJECT',
          name: 'State',
          description: null,
          fields: [
            {
              name: 'code',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'name',
              description: null,
              args: [],
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'SCALAR',
                  name: 'String',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'country',
              description: null,
              args: [],
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'OBJECT',
                  name: 'Country',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
          ],
          inputFields: null,
          interfaces: [],
          enumValues: null,
          possibleTypes: null,
        },
        {
          kind: 'INPUT_OBJECT',
          name: 'StringQueryOperatorInput',
          description: null,
          fields: null,
          inputFields: [
            {
              name: 'eq',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'ne',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'in',
              description: null,
              type: {
                kind: 'LIST',
                name: null,
                ofType: {
                  kind: 'SCALAR',
                  name: 'String',
                  ofType: null,
                },
              },
              defaultValue: null,
            },
            {
              name: 'nin',
              description: null,
              type: {
                kind: 'LIST',
                name: null,
                ofType: {
                  kind: 'SCALAR',
                  name: 'String',
                  ofType: null,
                },
              },
              defaultValue: null,
            },
            {
              name: 'regex',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'glob',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
          ],
          interfaces: null,
          enumValues: null,
          possibleTypes: null,
        },
        {
          kind: 'INPUT_OBJECT',
          name: 'CountryFilterInput',
          description: null,
          fields: null,
          inputFields: [
            {
              name: 'code',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'StringQueryOperatorInput',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'currency',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'StringQueryOperatorInput',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'continent',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'StringQueryOperatorInput',
                ofType: null,
              },
              defaultValue: null,
            },
          ],
          interfaces: null,
          enumValues: null,
          possibleTypes: null,
        },
        {
          kind: 'INPUT_OBJECT',
          name: 'ContinentFilterInput',
          description: null,
          fields: null,
          inputFields: [
            {
              name: 'code',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'StringQueryOperatorInput',
                ofType: null,
              },
              defaultValue: null,
            },
          ],
          interfaces: null,
          enumValues: null,
          possibleTypes: null,
        },
        {
          kind: 'INPUT_OBJECT',
          name: 'LanguageFilterInput',
          description: null,
          fields: null,
          inputFields: [
            {
              name: 'code',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'StringQueryOperatorInput',
                ofType: null,
              },
              defaultValue: null,
            },
          ],
          interfaces: null,
          enumValues: null,
          possibleTypes: null,
        },
        {
          kind: 'OBJECT',
          name: 'Query',
          description: null,
          fields: [
            {
              name: '_entities',
              description: null,
              args: [
                {
                  name: 'representations',
                  description: null,
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'LIST',
                      name: null,
                      ofType: {
                        kind: 'NON_NULL',
                        name: null,
                        ofType: {
                          kind: 'SCALAR',
                          name: '_Any',
                          ofType: null,
                        },
                      },
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'UNION',
                    name: '_Entity',
                    ofType: null,
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: '_service',
              description: null,
              args: [],
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'OBJECT',
                  name: '_Service',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'countries',
              description: null,
              args: [
                {
                  name: 'filter',
                  description: null,
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'CountryFilterInput',
                    ofType: null,
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'OBJECT',
                      name: 'Country',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'country',
              description: null,
              args: [
                {
                  name: 'code',
                  description: null,
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'SCALAR',
                      name: 'ID',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Country',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'continents',
              description: null,
              args: [
                {
                  name: 'filter',
                  description: null,
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'ContinentFilterInput',
                    ofType: null,
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'OBJECT',
                      name: 'Continent',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'continent',
              description: null,
              args: [
                {
                  name: 'code',
                  description: null,
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'SCALAR',
                      name: 'ID',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Continent',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'languages',
              description: null,
              args: [
                {
                  name: 'filter',
                  description: null,
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'LanguageFilterInput',
                    ofType: null,
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'OBJECT',
                      name: 'Language',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'language',
              description: null,
              args: [
                {
                  name: 'code',
                  description: null,
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'SCALAR',
                      name: 'ID',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Language',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
          ],
          inputFields: null,
          interfaces: [],
          enumValues: null,
          possibleTypes: null,
        },
        {
          kind: 'UNION',
          name: '_Entity',
          description: null,
          fields: null,
          inputFields: null,
          interfaces: null,
          enumValues: null,
          possibleTypes: [
            {
              kind: 'OBJECT',
              name: 'Country',
              ofType: null,
            },
            {
              kind: 'OBJECT',
              name: 'Continent',
              ofType: null,
            },
            {
              kind: 'OBJECT',
              name: 'Language',
              ofType: null,
            },
          ],
        },
        {
          kind: 'SCALAR',
          name: '_Any',
          description: null,
          fields: null,
          inputFields: null,
          interfaces: null,
          enumValues: null,
          possibleTypes: null,
        },
        {
          kind: 'OBJECT',
          name: '_Service',
          description: null,
          fields: [
            {
              name: 'sdl',
              description:
                'The sdl representing the federated service capabilities. Includes federation directives, removes federation types, and includes rest of full schema after schema directives have been applied',
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
          ],
          inputFields: null,
          interfaces: [],
          enumValues: null,
          possibleTypes: null,
        },
        {
          kind: 'OBJECT',
          name: '__Schema',
          description:
            'A GraphQL Schema defines the capabilities of a GraphQL server. It exposes all available types and directives on the server, as well as the entry points for query, mutation, and subscription operations.',
          fields: [
            {
              name: 'description',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'types',
              description: 'A list of all types supported by this server.',
              args: [],
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'OBJECT',
                      name: '__Type',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'queryType',
              description: 'The type that query operations will be rooted at.',
              args: [],
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'OBJECT',
                  name: '__Type',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'mutationType',
              description:
                'If this server supports mutation, the type that mutation operations will be rooted at.',
              args: [],
              type: {
                kind: 'OBJECT',
                name: '__Type',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'subscriptionType',
              description:
                'If this server support subscription, the type that subscription operations will be rooted at.',
              args: [],
              type: {
                kind: 'OBJECT',
                name: '__Type',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'directives',
              description: 'A list of all directives supported by this server.',
              args: [],
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'OBJECT',
                      name: '__Directive',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
          ],
          inputFields: null,
          interfaces: [],
          enumValues: null,
          possibleTypes: null,
        },
        {
          kind: 'OBJECT',
          name: '__Type',
          description:
            'The fundamental unit of any GraphQL Schema is the type. There are many kinds of types in GraphQL as represented by the `__TypeKind` enum.\n\nDepending on the kind of a type, certain fields describe information about that type. Scalar types provide no information beyond a name, description and optional `specifiedByUrl`, while Enum types provide their values. Object and Interface types provide the fields they describe. Abstract types, Union and Interface, provide the Object types possible at runtime. List and NonNull types compose other types.',
          fields: [
            {
              name: 'kind',
              description: null,
              args: [],
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'ENUM',
                  name: '__TypeKind',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'name',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'description',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'specifiedByUrl',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'fields',
              description: null,
              args: [
                {
                  name: 'includeDeprecated',
                  description: null,
                  type: {
                    kind: 'SCALAR',
                    name: 'Boolean',
                    ofType: null,
                  },
                  defaultValue: 'false',
                },
              ],
              type: {
                kind: 'LIST',
                name: null,
                ofType: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'OBJECT',
                    name: '__Field',
                    ofType: null,
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'interfaces',
              description: null,
              args: [],
              type: {
                kind: 'LIST',
                name: null,
                ofType: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'OBJECT',
                    name: '__Type',
                    ofType: null,
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'possibleTypes',
              description: null,
              args: [],
              type: {
                kind: 'LIST',
                name: null,
                ofType: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'OBJECT',
                    name: '__Type',
                    ofType: null,
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'enumValues',
              description: null,
              args: [
                {
                  name: 'includeDeprecated',
                  description: null,
                  type: {
                    kind: 'SCALAR',
                    name: 'Boolean',
                    ofType: null,
                  },
                  defaultValue: 'false',
                },
              ],
              type: {
                kind: 'LIST',
                name: null,
                ofType: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'OBJECT',
                    name: '__EnumValue',
                    ofType: null,
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'inputFields',
              description: null,
              args: [
                {
                  name: 'includeDeprecated',
                  description: null,
                  type: {
                    kind: 'SCALAR',
                    name: 'Boolean',
                    ofType: null,
                  },
                  defaultValue: 'false',
                },
              ],
              type: {
                kind: 'LIST',
                name: null,
                ofType: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'OBJECT',
                    name: '__InputValue',
                    ofType: null,
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'ofType',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: '__Type',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
          ],
          inputFields: null,
          interfaces: [],
          enumValues: null,
          possibleTypes: null,
        },
        {
          kind: 'ENUM',
          name: '__TypeKind',
          description:
            'An enum describing what kind of type a given `__Type` is.',
          fields: null,
          inputFields: null,
          interfaces: null,
          enumValues: [
            {
              name: 'SCALAR',
              description: 'Indicates this type is a scalar.',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'OBJECT',
              description:
                'Indicates this type is an object. `fields` and `interfaces` are valid fields.',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'INTERFACE',
              description:
                'Indicates this type is an interface. `fields`, `interfaces`, and `possibleTypes` are valid fields.',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'UNION',
              description:
                'Indicates this type is a union. `possibleTypes` is a valid field.',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'ENUM',
              description:
                'Indicates this type is an enum. `enumValues` is a valid field.',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'INPUT_OBJECT',
              description:
                'Indicates this type is an input object. `inputFields` is a valid field.',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'LIST',
              description:
                'Indicates this type is a list. `ofType` is a valid field.',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'NON_NULL',
              description:
                'Indicates this type is a non-null. `ofType` is a valid field.',
              isDeprecated: false,
              deprecationReason: null,
            },
          ],
          possibleTypes: null,
        },
        {
          kind: 'OBJECT',
          name: '__Field',
          description:
            'Object and Interface types are described by a list of Fields, each of which has a name, potentially a list of arguments, and a return type.',
          fields: [
            {
              name: 'name',
              description: null,
              args: [],
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'SCALAR',
                  name: 'String',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'description',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'args',
              description: null,
              args: [
                {
                  name: 'includeDeprecated',
                  description: null,
                  type: {
                    kind: 'SCALAR',
                    name: 'Boolean',
                    ofType: null,
                  },
                  defaultValue: 'false',
                },
              ],
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'OBJECT',
                      name: '__InputValue',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'type',
              description: null,
              args: [],
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'OBJECT',
                  name: '__Type',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'isDeprecated',
              description: null,
              args: [],
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'SCALAR',
                  name: 'Boolean',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'deprecationReason',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
          ],
          inputFields: null,
          interfaces: [],
          enumValues: null,
          possibleTypes: null,
        },
        {
          kind: 'OBJECT',
          name: '__InputValue',
          description:
            'Arguments provided to Fields or Directives and the input fields of an InputObject are represented as Input Values which describe their type and optionally a default value.',
          fields: [
            {
              name: 'name',
              description: null,
              args: [],
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'SCALAR',
                  name: 'String',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'description',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'type',
              description: null,
              args: [],
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'OBJECT',
                  name: '__Type',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'defaultValue',
              description:
                'A GraphQL-formatted string representing the default value for this input value.',
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'isDeprecated',
              description: null,
              args: [],
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'SCALAR',
                  name: 'Boolean',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'deprecationReason',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
          ],
          inputFields: null,
          interfaces: [],
          enumValues: null,
          possibleTypes: null,
        },
        {
          kind: 'OBJECT',
          name: '__EnumValue',
          description:
            'One possible value for a given Enum. Enum values are unique values, not a placeholder for a string or numeric value. However an Enum value is returned in a JSON response as a string.',
          fields: [
            {
              name: 'name',
              description: null,
              args: [],
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'SCALAR',
                  name: 'String',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'description',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'isDeprecated',
              description: null,
              args: [],
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'SCALAR',
                  name: 'Boolean',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'deprecationReason',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
          ],
          inputFields: null,
          interfaces: [],
          enumValues: null,
          possibleTypes: null,
        },
        {
          kind: 'OBJECT',
          name: '__Directive',
          description:
            "A Directive provides a way to describe alternate runtime execution and type validation behavior in a GraphQL document.\n\nIn some cases, you need to provide options to alter GraphQL's execution behavior in ways field arguments will not suffice, such as conditionally including or skipping a field. Directives provide this by describing additional information to the executor.",
          fields: [
            {
              name: 'name',
              description: null,
              args: [],
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'SCALAR',
                  name: 'String',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'description',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'isRepeatable',
              description: null,
              args: [],
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'SCALAR',
                  name: 'Boolean',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'locations',
              description: null,
              args: [],
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'ENUM',
                      name: '__DirectiveLocation',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'args',
              description: null,
              args: [
                {
                  name: 'includeDeprecated',
                  description: null,
                  type: {
                    kind: 'SCALAR',
                    name: 'Boolean',
                    ofType: null,
                  },
                  defaultValue: 'false',
                },
              ],
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'OBJECT',
                      name: '__InputValue',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
          ],
          inputFields: null,
          interfaces: [],
          enumValues: null,
          possibleTypes: null,
        },
        {
          kind: 'ENUM',
          name: '__DirectiveLocation',
          description:
            'A Directive can be adjacent to many parts of the GraphQL language, a __DirectiveLocation describes one such possible adjacencies.',
          fields: null,
          inputFields: null,
          interfaces: null,
          enumValues: [
            {
              name: 'QUERY',
              description: 'Location adjacent to a query operation.',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'MUTATION',
              description: 'Location adjacent to a mutation operation.',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'SUBSCRIPTION',
              description: 'Location adjacent to a subscription operation.',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'FIELD',
              description: 'Location adjacent to a field.',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'FRAGMENT_DEFINITION',
              description: 'Location adjacent to a fragment definition.',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'FRAGMENT_SPREAD',
              description: 'Location adjacent to a fragment spread.',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'INLINE_FRAGMENT',
              description: 'Location adjacent to an inline fragment.',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'VARIABLE_DEFINITION',
              description: 'Location adjacent to a variable definition.',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'SCHEMA',
              description: 'Location adjacent to a schema definition.',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'SCALAR',
              description: 'Location adjacent to a scalar definition.',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'OBJECT',
              description: 'Location adjacent to an object type definition.',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'FIELD_DEFINITION',
              description: 'Location adjacent to a field definition.',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'ARGUMENT_DEFINITION',
              description: 'Location adjacent to an argument definition.',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'INTERFACE',
              description: 'Location adjacent to an interface definition.',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'UNION',
              description: 'Location adjacent to a union definition.',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'ENUM',
              description: 'Location adjacent to an enum definition.',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'ENUM_VALUE',
              description: 'Location adjacent to an enum value definition.',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'INPUT_OBJECT',
              description:
                'Location adjacent to an input object type definition.',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'INPUT_FIELD_DEFINITION',
              description:
                'Location adjacent to an input object field definition.',
              isDeprecated: false,
              deprecationReason: null,
            },
          ],
          possibleTypes: null,
        },
      ],
      directives: [
        {
          name: 'include',
          description:
            'Directs the executor to include this field or fragment only when the `if` argument is true.',
          locations: ['FIELD', 'FRAGMENT_SPREAD', 'INLINE_FRAGMENT'],
          args: [
            {
              name: 'if',
              description: 'Included when true.',
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'SCALAR',
                  name: 'Boolean',
                  ofType: null,
                },
              },
              defaultValue: null,
            },
          ],
        },
        {
          name: 'skip',
          description:
            'Directs the executor to skip this field or fragment when the `if` argument is true.',
          locations: ['FIELD', 'FRAGMENT_SPREAD', 'INLINE_FRAGMENT'],
          args: [
            {
              name: 'if',
              description: 'Skipped when true.',
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'SCALAR',
                  name: 'Boolean',
                  ofType: null,
                },
              },
              defaultValue: null,
            },
          ],
        },
        {
          name: 'deprecated',
          description:
            'Marks an element of a GraphQL schema as no longer supported.',
          locations: [
            'FIELD_DEFINITION',
            'ARGUMENT_DEFINITION',
            'INPUT_FIELD_DEFINITION',
            'ENUM_VALUE',
          ],
          args: [
            {
              name: 'reason',
              description:
                'Explains why this element was deprecated, usually also including a suggestion for how to access supported similar data. Formatted using the Markdown syntax, as specified by [CommonMark](https://commonmark.org/).',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: '"No longer supported"',
            },
          ],
        },
        {
          name: 'specifiedBy',
          description:
            'Exposes a URL that specifies the behaviour of this scalar.',
          locations: ['SCALAR'],
          args: [
            {
              name: 'url',
              description:
                'The URL that specifies the behaviour of this scalar.',
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'SCALAR',
                  name: 'String',
                  ofType: null,
                },
              },
              defaultValue: null,
            },
          ],
        },
        {
          name: 'key',
          description: null,
          locations: ['OBJECT', 'INTERFACE'],
          args: [
            {
              name: 'fields',
              description: null,
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'SCALAR',
                  name: 'String',
                  ofType: null,
                },
              },
              defaultValue: null,
            },
          ],
        },
        {
          name: 'extends',
          description: null,
          locations: ['OBJECT', 'INTERFACE'],
          args: [],
        },
        {
          name: 'external',
          description: null,
          locations: ['OBJECT', 'FIELD_DEFINITION'],
          args: [],
        },
        {
          name: 'requires',
          description: null,
          locations: ['FIELD_DEFINITION'],
          args: [
            {
              name: 'fields',
              description: null,
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'SCALAR',
                  name: 'String',
                  ofType: null,
                },
              },
              defaultValue: null,
            },
          ],
        },
        {
          name: 'provides',
          description: null,
          locations: ['FIELD_DEFINITION'],
          args: [
            {
              name: 'fields',
              description: null,
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'SCALAR',
                  name: 'String',
                  ofType: null,
                },
              },
              defaultValue: null,
            },
          ],
        },
      ],
    },
  },
};
