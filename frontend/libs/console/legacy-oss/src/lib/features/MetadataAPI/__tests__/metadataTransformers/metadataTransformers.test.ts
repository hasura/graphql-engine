import { MetadataTransformer } from '../../hooks/metadataTransformers';
import {
  dbToLocalDbRelationship,
  dbToRemoteSchemaRelationships,
  tableRelationships,
} from './__fixtures__';

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
        "relationshipName": "t",
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

test('transformDbToLocal returns object relationships correctly', () => {
  const result = MetadataTransformer.transformTableRelationships({
    target: { table: 'product', schema: 'public', database: 'default' },
    relationships: {
      objectRelationships: dbToLocalDbRelationship.objectRelationships,
      arrayRelationships: [],
    },
    tableRelationships,
  });

  expect(result).toMatchInlineSnapshot(`
    Array [
      Object {
        "comment": undefined,
        "from": Object {
          "column": Array [
            "fk_user_id",
          ],
          "table": "product",
        },
        "name": "product_user",
        "to": Object {
          "column": Array [
            "id",
          ],
          "table": "\\"user\\"",
        },
        "type": "object",
      },
    ]
  `);
});
