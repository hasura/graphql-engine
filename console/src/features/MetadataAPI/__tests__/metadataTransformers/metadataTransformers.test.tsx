import { MetadataTransformer } from '../../hooks/metadataTransformers';
import { dbToRemoteSchemaRelationships } from './__fixtures__';

test('transformDbToRemoteSchema returns new and legacy formats consistently', () => {
  const result = MetadataTransformer.transformDbToRemoteSchema(
    dbToRemoteSchemaRelationships
  );

  expect(result).toMatchInlineSnapshot(`
    Array [
      Object {
        "lhs_fields": Array [
          "id",
        ],
        "relationshipName": "new_payload",
        "remoteSchemaName": "remoteSchema3",
        "remote_field": Object {
          "test": Object {
            "arguments": Object {
              "where": Object {
                "id": Object {
                  "_eq": "$id",
                },
              },
            },
          },
        },
        "target": Object {
          "database": "default",
          "schema": "public",
          "table": "user",
        },
      },
      Object {
        "lhs_fields": Array [
          "id",
        ],
        "relationshipName": "old_payload",
        "remoteSchemaName": "remoteSchema3",
        "remote_field": Object {
          "test": Object {
            "arguments": Object {
              "where": Object {
                "id": Object {
                  "_eq": "$id",
                },
              },
            },
          },
        },
        "target": Object {
          "database": "default",
          "schema": "public",
          "table": "user",
        },
      },
    ]
  `);
});
