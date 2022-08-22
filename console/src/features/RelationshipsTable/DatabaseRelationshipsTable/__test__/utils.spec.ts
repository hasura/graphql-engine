import {
  expectedRemoteSchemaRelationshipOutput,
  expectedLegacyRemoteSchemaRelationshipsOutput,
  expectedRemoteDBRelationshipOutput,
  expectedManualLocalRelationshipOutput,
  expectedLocalTableRelationships,
  expectedSameTableObjectRelationships,
} from './mocks';
import {
  adaptRemoteSchemaRelationship,
  adaptLegacyRemoteSchemaRelationship,
  adaptRemoteDBRelationship,
  adaptManualRelationship,
  adaptLocalTableRelationship,
  adaptSameTableObjectRelationship,
} from '../hooks/useListAllRelationshipsFromMetadata/utils';

describe('test adapters', () => {
  it('for remote schema relationship', async () => {
    const result = adaptRemoteSchemaRelationship(
      'chinook',
      { name: 'Employee', schema: 'public' },
      {
        definition: {
          to_remote_schema: {
            lhs_fields: ['EmployeeId'],
            remote_field: {
              country: {
                arguments: {
                  code: '$EmployeeId',
                },
              },
            },
            remote_schema: 'rs',
          },
        },
        name: 'new_rs_to_db',
      }
    );
    expect(result).toEqual(expectedRemoteSchemaRelationshipOutput);
  });

  it('for legacy remote schema relationship', async () => {
    const result = adaptLegacyRemoteSchemaRelationship(
      'chinook',
      { name: 'Employee', schema: 'public' },

      {
        definition: {
          hasura_fields: ['EmployeeId'],
          remote_field: {
            country: {
              arguments: {
                code: '$EmployeeId',
              },
            },
          },
          remote_schema: 'rs',
        },
        name: 'legacy_db_to_rs',
      }
    );
    expect(result).toEqual(expectedLegacyRemoteSchemaRelationshipsOutput);
  });

  it('for remote db relationship', async () => {
    const result = adaptRemoteDBRelationship(
      'chinook',
      { name: 'Employee', schema: 'public' },
      {
        definition: {
          to_source: {
            field_mapping: {
              EmployeeId: 'brand_id',
            },
            relationship_type: 'object',
            source: 'bikes',
            table: {
              name: 'brands',
              schema: 'production',
            },
          },
        },
        name: 'remote_db_object_rel',
      }
    );
    expect(result).toEqual(expectedRemoteDBRelationshipOutput);
  });

  it('for local relationships created manually', async () => {
    const result = adaptManualRelationship(
      'chinook',
      { name: 'Employee', schema: 'public' },
      {
        name: 'local_array_rel',
        using: {
          manual_configuration: {
            column_mapping: {
              EmployeeId: 'AlbumId',
            },
            insertion_order: null,
            remote_table: {
              name: 'Album',
              schema: 'public',
            },
          },
        },
      }
    );
    expect(result).toEqual(expectedManualLocalRelationshipOutput);
  });

  it('for local relationships created via foreign keys', async () => {
    const result = adaptLocalTableRelationship(
      'chinook',
      { name: 'Employee', schema: 'public' },
      {
        name: 'Employees',
        using: {
          foreign_key_constraint_on: {
            column: 'ReportsTo',
            table: {
              name: 'Employee',
              schema: 'public',
            },
          },
        },
      },
      [
        {
          from: {
            table: 'Employee',
            column: ['ReportsTo'],
          },
          to: {
            table: 'Employee',
            column: ['EmployeeId'],
          },
        },
        {
          from: {
            table: 'Customer',
            column: ['SupportRepId'],
          },
          to: {
            table: 'Employee',
            column: ['EmployeeId'],
          },
        },
      ]
    );
    expect(result).toEqual(expectedLocalTableRelationships);
  });

  it('for same table relationships created via foreign keys', async () => {
    const result = adaptSameTableObjectRelationship(
      'chinook',
      { name: 'Employee', schema: 'public' },
      {
        name: 'Employee',
        using: {
          foreign_key_constraint_on: 'ReportsTo',
        },
      },
      [
        {
          from: {
            table: 'Employee',
            column: ['ReportsTo'],
          },
          to: {
            table: 'Employee',
            column: ['EmployeeId'],
          },
        },
        {
          from: {
            table: 'Customer',
            column: ['SupportRepId'],
          },
          to: {
            table: 'Employee',
            column: ['EmployeeId'],
          },
        },
      ]
    );
    expect(result).toEqual(expectedSameTableObjectRelationships);
  });
});
