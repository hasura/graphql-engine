import { MetadataResponse } from '@/features/MetadataAPI';

export const metadata: MetadataResponse = {
  resource_version: 2,
  metadata: {
    version: 3,
    inherited_roles: [],
    sources: [
      {
        name: 'default',
        kind: 'postgres',
        tables: [
          {
            table: { name: 'test', schema: 'public' },
            insert_permissions: [
              { role: 'manager', permission: { check: {}, columns: [] } },
              { role: 'users', permission: { check: {}, columns: ['id'] } },
            ],
          },
        ],
        configuration: {
          connection_info: {
            use_prepared_statements: true,
            database_url: {
              from_env: 'HASURA_GRAPHQL_DATABASE_URL',
            },
            isolation_level: 'read-committed',
            pool_settings: {
              connection_lifetime: 600,
              retries: 1,
              idle_timeout: 180,
              max_connections: 50,
            },
          },
        },
      },
    ],
    remote_schemas: [
      {
        name: 'RS1',
        definition: {
          url: 'https://graphql-pokemon2.vercel.app',
          timeout_seconds: 60,
        },
        permissions: [
          {
            role: 'rs_role',
            definition: {
              schema:
                'schema  { query: Query }\n\ntype Attack { damage: Int\n  name: String\n  type: String\n}\n\ntype Pokemon { attacks: PokemonAttack\n  classification: String\n  evolutionRequirements: PokemonEvolutionRequirement\n  evolutions: [Pokemon]\n  fleeRate: Float\n  height: PokemonDimension\n  id: ID!\n  image: String\n  maxCP: Int\n  maxHP: Int\n  name: String\n  number: String\n  resistant: [String]\n  types: [String]\n  weaknesses: [String]\n  weight: PokemonDimension\n}\n\ntype PokemonAttack { fast: [Attack]\n  special: [Attack]\n}\n\ntype PokemonDimension { maximum: String\n  minimum: String\n}\n\ntype PokemonEvolutionRequirement { amount: Int\n  name: String\n}\n\ntype Query { pokemon(id: String @preset(value: "X-Hasura-User-Id"), name: String @preset(value: "X-Hasura-User-Id")): Pokemon\n  pokemons(first: Int!): [Pokemon]\n  query: Query\n}',
            },
            remote_schema_name: '',
            comment: null,
          },
        ],
      },
    ],
    query_collections: [
      {
        name: 'allowed-queries',
        definition: {
          queries: [
            {
              name: 'introspection query',
              query:
                'query IntrospectionQuery {\n      __schema {\n        queryType { name }\n        mutationType { name }\n        subscriptionType { name }\n        types {\n          ...FullType\n        }\n        directives {\n          name\n          description\n          locations\n          args {\n            ...InputValue\n          }\n        }\n      }\n    }\n\n    fragment FullType on __Type {\n      kind\n      name\n      description\n      fields(includeDeprecated: true) {\n        name\n        description\n        args {\n          ...InputValue\n        }\n        type {\n          ...TypeRef\n        }\n        isDeprecated\n        deprecationReason\n      }\n      inputFields {\n        ...InputValue\n      }\n      interfaces {\n        ...TypeRef\n      }\n      enumValues(includeDeprecated: true) {\n        name\n        description\n        isDeprecated\n        deprecationReason\n      }\n      possibleTypes {\n        ...TypeRef\n      }\n    }\n\n    fragment InputValue on __InputValue {\n      name\n      description\n      type { ...TypeRef }\n      defaultValue\n    }\n\n    fragment TypeRef on __Type {\n      kind\n      name\n      ofType {\n        kind\n        name\n        ofType {\n          kind\n          name\n          ofType {\n            kind\n            name\n            ofType {\n              kind\n              name\n              ofType {\n                kind\n                name\n                ofType {\n                  kind\n                  name\n                  ofType {\n                    kind\n                    name\n                  }\n                }\n              }\n            }\n          }\n        }\n      }\n    }',
            },
          ],
        },
      },
    ],
    allowlist: [
      {
        collection: 'allowed-queries',
        scope: {
          global: false,
          roles: ['manager'],
        },
      },
      {
        collection: 'admin-only-queries',
        scope: {
          global: false,
          roles: ['users'],
        },
      },
      {
        collection: 'rest-endpoint',
        scope: {
          global: false,
          roles: ['users', 'manager'],
        },
      },
    ],
  },
};

export const metadata_with_no_query_collections: MetadataResponse = {
  resource_version: 48,
  metadata: {
    version: 3,
    sources: [],
    remote_schemas: [],
    inherited_roles: [],
  },
};
