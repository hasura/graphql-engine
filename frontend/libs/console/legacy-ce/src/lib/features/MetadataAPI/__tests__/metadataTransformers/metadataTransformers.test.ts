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
    [
      {
        "lhs_fields": [
          "id",
        ],
        "relationshipName": "new_payload",
        "remoteSchemaName": "remoteSchema3",
        "remote_field": {
          "test": {
            "arguments": {
              "where": {
                "id": {
                  "_eq": "$id",
                },
              },
            },
          },
        },
        "target": {
          "database": "default",
          "schema": "public",
          "table": "user",
        },
      },
      {
        "lhs_fields": [
          "id",
        ],
        "relationshipName": "t",
        "remoteSchemaName": "remoteSchema3",
        "remote_field": {
          "test": {
            "arguments": {
              "where": {
                "id": {
                  "_eq": "$id",
                },
              },
            },
          },
        },
        "target": {
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
    [
      {
        "comment": undefined,
        "from": {
          "column": [
            "fk_user_id",
          ],
          "table": "product",
        },
        "name": "product_user",
        "to": {
          "column": [
            "id",
          ],
          "table": ""user"",
        },
        "type": "object",
      },
    ]
  `);
});
