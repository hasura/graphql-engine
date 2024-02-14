import {
  filterBySchema,
  getForeignKeyRelationships,
  getTrackedSuggestedRelationships,
} from './selectors';
import {
  chinook_data_set_suggested_relationships_from_api,
  mock_composite_fk_suggested_relationships_from_api,
} from '../mock';

// basic test to make sure manual config rels get filtered out.
describe('getForeignKeyRelationships gets all the FK relationships', () => {
  it('for Album Table with both fk and manual rels configured', () => {
    const result = getForeignKeyRelationships({
      table: {
        name: 'Album',
        schema: 'public',
      },
      object_relationships: [
        {
          name: 'AlbumArtist',
          using: {
            manual_configuration: {
              column_mapping: {
                ArtistId: 'ArtistId',
              },
              insertion_order: null,
              remote_table: {
                name: 'Artist',
                schema: 'public',
              },
            },
          },
        },
        {
          name: 'Album_Artist',
          using: {
            foreign_key_constraint_on: 'ArtistId',
          },
        },
      ],
      array_relationships: [
        {
          name: 'AlbumTracks',
          using: {
            manual_configuration: {
              column_mapping: {
                AlbumId: 'AlbumId',
              },
              remote_table: {
                name: 'Track',
                schema: 'public',
              },
            },
          },
        },
        {
          name: 'Album_Tracks',
          using: {
            foreign_key_constraint_on: {
              column: 'AlbumId',
              table: {
                name: 'Track',
                schema: 'public',
              },
            },
          },
        },
      ],
    });
    const expectedResult = [
      {
        table: {
          name: 'Album',
          schema: 'public',
        },
        name: 'Album_Artist',
        using: {
          foreign_key_constraint_on: 'ArtistId',
        },
        type: 'object',
      },
      {
        table: {
          name: 'Album',
          schema: 'public',
        },
        name: 'Album_Tracks',
        using: {
          foreign_key_constraint_on: {
            column: 'AlbumId',
            table: {
              name: 'Track',
              schema: 'public',
            },
          },
        },
        type: 'array',
      },
    ];
    expect(result).toStrictEqual(expectedResult);
  });
  it('when Artist table has all fk rels tracked', () => {
    const result = getForeignKeyRelationships({
      table: {
        name: 'Artist',
        schema: 'public',
      },
      array_relationships: [
        {
          name: 'Artist_Albums',
          using: {
            foreign_key_constraint_on: {
              column: 'ArtistId',
              table: {
                name: 'Album',
                schema: 'public',
              },
            },
          },
        },
      ],
    });
    const expectedResult = [
      {
        table: {
          name: 'Artist',
          schema: 'public',
        },
        type: 'array',
        name: 'Artist_Albums',
        using: {
          foreign_key_constraint_on: {
            column: 'ArtistId',
            table: {
              name: 'Album',
              schema: 'public',
            },
          },
        },
      },
    ];
    expect(result).toStrictEqual(expectedResult);
  });
});

describe('getTrackedSuggestedRelationships', () => {
  describe('works for the chinook dataset', () => {
    it('[OBJECT] Album_Artist', () => {
      const result = getTrackedSuggestedRelationships({
        suggestedRelationships:
          chinook_data_set_suggested_relationships_from_api,
        fkConstraintRelationships: [
          {
            name: 'Album_Artist',
            using: {
              foreign_key_constraint_on: 'ArtistId',
            },
            table: {
              name: 'Album',
              schema: 'public',
            },
            type: 'object',
          },
        ],
      });
      expect(result).toStrictEqual([
        {
          name: 'Album_Artist',
          fromTable: {
            name: 'Album',
            schema: 'public',
          },
          toTable: {
            name: 'Artist',
            schema: 'public',
          },
          columnMapping: {
            ArtistId: 'ArtistId',
          },
          type: 'object',
        },
      ]);
    });

    it('[OBJECT] Customer_Employee', () => {
      const result = getTrackedSuggestedRelationships({
        suggestedRelationships:
          chinook_data_set_suggested_relationships_from_api,
        fkConstraintRelationships: [
          {
            name: 'Customer_Employee',
            using: {
              foreign_key_constraint_on: 'SupportRepId',
            },
            table: {
              name: 'Customer',
              schema: 'public',
            },
            type: 'object',
          },
        ],
      });
      expect(result).toStrictEqual([
        {
          name: 'Customer_Employee',
          fromTable: {
            name: 'Customer',
            schema: 'public',
          },
          toTable: {
            name: 'Employee',
            schema: 'public',
          },
          columnMapping: {
            SupportRepId: 'EmployeeId',
          },
          type: 'object',
        },
      ]);
    });

    it('[OBJECT] Employee_Employee', () => {
      const result = getTrackedSuggestedRelationships({
        suggestedRelationships:
          chinook_data_set_suggested_relationships_from_api,
        fkConstraintRelationships: [
          {
            name: 'Employee_Employee',
            using: {
              foreign_key_constraint_on: 'ReportsTo',
            },
            table: {
              name: 'Employee',
              schema: 'public',
            },
            type: 'object',
          },
        ],
      });
      expect(result).toStrictEqual([
        {
          name: 'Employee_Employee',
          fromTable: {
            name: 'Employee',
            schema: 'public',
          },
          toTable: {
            name: 'Employee',
            schema: 'public',
          },
          columnMapping: {
            ReportsTo: 'EmployeeId',
          },
          type: 'object',
        },
      ]);
    });

    it('[OBJECT] Invoice_Customer', () => {
      const result = getTrackedSuggestedRelationships({
        suggestedRelationships:
          chinook_data_set_suggested_relationships_from_api,
        fkConstraintRelationships: [
          {
            name: 'Invoice_Customer',
            using: {
              foreign_key_constraint_on: 'CustomerId',
            },
            table: {
              name: 'Invoice',
              schema: 'public',
            },
            type: 'object',
          },
        ],
      });
      expect(result).toStrictEqual([
        {
          name: 'Invoice_Customer',
          fromTable: {
            name: 'Invoice',
            schema: 'public',
          },
          toTable: {
            name: 'Customer',
            schema: 'public',
          },
          columnMapping: {
            CustomerId: 'CustomerId',
          },
          type: 'object',
        },
      ]);
    });

    it('[OBJECT] InvoiceLine_Invoice', () => {
      const result = getTrackedSuggestedRelationships({
        suggestedRelationships:
          chinook_data_set_suggested_relationships_from_api,
        fkConstraintRelationships: [
          {
            name: 'InvoiceLine_Invoice',
            using: {
              foreign_key_constraint_on: 'InvoiceId',
            },
            table: {
              name: 'InvoiceLine',
              schema: 'public',
            },
            type: 'object',
          },
        ],
      });
      expect(result).toStrictEqual([
        {
          name: 'InvoiceLine_Invoice',
          fromTable: {
            name: 'InvoiceLine',
            schema: 'public',
          },
          toTable: {
            name: 'Invoice',
            schema: 'public',
          },
          columnMapping: {
            InvoiceId: 'InvoiceId',
          },
          type: 'object',
        },
      ]);
    });

    it('[OBJECT] InvoiceLine_Track', () => {
      const result = getTrackedSuggestedRelationships({
        suggestedRelationships:
          chinook_data_set_suggested_relationships_from_api,
        fkConstraintRelationships: [
          {
            name: 'InvoiceLine_Track',
            using: {
              foreign_key_constraint_on: 'TrackId',
            },
            table: {
              name: 'InvoiceLine',
              schema: 'public',
            },
            type: 'object',
          },
        ],
      });
      expect(result).toStrictEqual([
        {
          name: 'InvoiceLine_Track',
          fromTable: {
            name: 'InvoiceLine',
            schema: 'public',
          },
          toTable: {
            name: 'Track',
            schema: 'public',
          },
          columnMapping: {
            TrackId: 'TrackId',
          },
          type: 'object',
        },
      ]);
    });

    it('[OBJECT] InvoiceLine_Track', () => {
      const result = getTrackedSuggestedRelationships({
        suggestedRelationships:
          chinook_data_set_suggested_relationships_from_api,
        fkConstraintRelationships: [
          {
            name: 'PlaylistTrack_Playlist',
            using: {
              foreign_key_constraint_on: 'PlaylistId',
            },
            table: {
              name: 'PlaylistTrack',
              schema: 'public',
            },
            type: 'object',
          },
        ],
      });
      expect(result).toStrictEqual([
        {
          name: 'PlaylistTrack_Playlist',
          fromTable: {
            name: 'PlaylistTrack',
            schema: 'public',
          },
          toTable: {
            name: 'Playlist',
            schema: 'public',
          },
          columnMapping: {
            PlaylistId: 'PlaylistId',
          },
          type: 'object',
        },
      ]);
    });

    it('[OBJECT] PlaylistTrack_Track', () => {
      const result = getTrackedSuggestedRelationships({
        suggestedRelationships:
          chinook_data_set_suggested_relationships_from_api,
        fkConstraintRelationships: [
          {
            name: 'PlaylistTrack_Track',
            using: {
              foreign_key_constraint_on: 'TrackId',
            },
            table: {
              name: 'PlaylistTrack',
              schema: 'public',
            },
            type: 'object',
          },
        ],
      });
      expect(result).toStrictEqual([
        {
          name: 'PlaylistTrack_Track',
          fromTable: {
            name: 'PlaylistTrack',
            schema: 'public',
          },
          toTable: {
            name: 'Track',
            schema: 'public',
          },
          columnMapping: {
            TrackId: 'TrackId',
          },
          type: 'object',
        },
      ]);
    });

    it('[OBJECT] PlaylistTrack_Playlist', () => {
      const result = getTrackedSuggestedRelationships({
        suggestedRelationships:
          chinook_data_set_suggested_relationships_from_api,
        fkConstraintRelationships: [
          {
            name: 'PlaylistTrack_Playlist',
            using: {
              foreign_key_constraint_on: 'PlaylistId',
            },
            table: {
              name: 'PlaylistTrack',
              schema: 'public',
            },
            type: 'object',
          },
        ],
      });
      expect(result).toStrictEqual([
        {
          name: 'PlaylistTrack_Playlist',
          fromTable: {
            name: 'PlaylistTrack',
            schema: 'public',
          },
          toTable: {
            name: 'Playlist',
            schema: 'public',
          },
          columnMapping: {
            PlaylistId: 'PlaylistId',
          },
          type: 'object',
        },
      ]);
    });

    it('[OBJECT] Track_Album', () => {
      const result = getTrackedSuggestedRelationships({
        suggestedRelationships:
          chinook_data_set_suggested_relationships_from_api,
        fkConstraintRelationships: [
          {
            name: 'Track_Album',
            using: {
              foreign_key_constraint_on: 'AlbumId',
            },
            table: {
              name: 'Track',
              schema: 'public',
            },
            type: 'object',
          },
        ],
      });
      expect(result).toStrictEqual([
        {
          name: 'Track_Album',
          fromTable: {
            name: 'Track',
            schema: 'public',
          },
          toTable: {
            name: 'Album',
            schema: 'public',
          },
          columnMapping: {
            AlbumId: 'AlbumId',
          },
          type: 'object',
        },
      ]);
    });

    it('[OBJECT] Track_Genre', () => {
      const result = getTrackedSuggestedRelationships({
        suggestedRelationships:
          chinook_data_set_suggested_relationships_from_api,
        fkConstraintRelationships: [
          {
            name: 'Track_Genre',
            using: {
              foreign_key_constraint_on: 'GenreId',
            },
            table: {
              name: 'Track',
              schema: 'public',
            },
            type: 'object',
          },
        ],
      });
      expect(result).toStrictEqual([
        {
          name: 'Track_Genre',
          fromTable: {
            name: 'Track',
            schema: 'public',
          },
          toTable: {
            name: 'Genre',
            schema: 'public',
          },
          columnMapping: {
            GenreId: 'GenreId',
          },
          type: 'object',
        },
      ]);
    });

    it('[OBJECT] Track_MediaType', () => {
      const result = getTrackedSuggestedRelationships({
        suggestedRelationships:
          chinook_data_set_suggested_relationships_from_api,
        fkConstraintRelationships: [
          {
            name: 'Track_MediaType',
            using: {
              foreign_key_constraint_on: 'MediaTypeId',
            },
            table: {
              name: 'Track',
              schema: 'public',
            },
            type: 'object',
          },
        ],
      });
      expect(result).toStrictEqual([
        {
          name: 'Track_MediaType',
          fromTable: {
            name: 'Track',
            schema: 'public',
          },
          toTable: {
            name: 'MediaType',
            schema: 'public',
          },
          columnMapping: {
            MediaTypeId: 'MediaTypeId',
          },
          type: 'object',
        },
      ]);
    });

    it('[ARRAY] Album_Tracks', () => {
      const result = getTrackedSuggestedRelationships({
        suggestedRelationships:
          chinook_data_set_suggested_relationships_from_api,
        fkConstraintRelationships: [
          {
            name: 'Album_Tracks',
            using: {
              foreign_key_constraint_on: {
                column: 'AlbumId',
                table: {
                  name: 'Track',
                  schema: 'public',
                },
              },
            },
            table: {
              name: 'Album',
              schema: 'public',
            },
            type: 'array',
          },
        ],
      });
      expect(result).toStrictEqual([
        {
          name: 'Album_Tracks',
          fromTable: {
            name: 'Album',
            schema: 'public',
          },
          toTable: {
            name: 'Track',
            schema: 'public',
          },
          columnMapping: {
            AlbumId: 'AlbumId',
          },
          type: 'array',
        },
      ]);
    });

    it('[ARRAY] Artist_Albums', () => {
      const result = getTrackedSuggestedRelationships({
        suggestedRelationships:
          chinook_data_set_suggested_relationships_from_api,
        fkConstraintRelationships: [
          {
            name: 'Artist_Albums',
            using: {
              foreign_key_constraint_on: {
                column: 'ArtistId',
                table: {
                  name: 'Album',
                  schema: 'public',
                },
              },
            },
            table: {
              name: 'Artist',
              schema: 'public',
            },
            type: 'array',
          },
        ],
      });
      expect(result).toStrictEqual([
        {
          name: 'Artist_Albums',
          fromTable: {
            name: 'Artist',
            schema: 'public',
          },
          toTable: {
            name: 'Album',
            schema: 'public',
          },
          columnMapping: {
            ArtistId: 'ArtistId',
          },
          type: 'array',
        },
      ]);
    });

    it('[ARRAY] Customer_Invoices', () => {
      const result = getTrackedSuggestedRelationships({
        suggestedRelationships:
          chinook_data_set_suggested_relationships_from_api,
        fkConstraintRelationships: [
          {
            name: 'Customer_Invoices',
            using: {
              foreign_key_constraint_on: {
                column: 'CustomerId',
                table: {
                  name: 'Invoice',
                  schema: 'public',
                },
              },
            },
            table: {
              name: 'Customer',
              schema: 'public',
            },
            type: 'array',
          },
        ],
      });
      expect(result).toStrictEqual([
        {
          name: 'Customer_Invoices',
          fromTable: {
            name: 'Customer',
            schema: 'public',
          },
          toTable: {
            name: 'Invoice',
            schema: 'public',
          },
          columnMapping: {
            CustomerId: 'CustomerId',
          },
          type: 'array',
        },
      ]);
    });

    it('[ARRAY] Employee_Customers', () => {
      const result = getTrackedSuggestedRelationships({
        suggestedRelationships:
          chinook_data_set_suggested_relationships_from_api,
        fkConstraintRelationships: [
          {
            name: 'Employee_Customers',
            using: {
              foreign_key_constraint_on: {
                column: 'SupportRepId',
                table: {
                  name: 'Customer',
                  schema: 'public',
                },
              },
            },
            table: {
              name: 'Employee',
              schema: 'public',
            },
            type: 'array',
          },
        ],
      });
      expect(result).toStrictEqual([
        {
          name: 'Employee_Customers',
          fromTable: {
            name: 'Employee',
            schema: 'public',
          },
          toTable: {
            name: 'Customer',
            schema: 'public',
          },
          columnMapping: {
            EmployeeId: 'SupportRepId',
          },
          type: 'array',
        },
      ]);
    });

    it('[ARRAY] Employee_Employees', () => {
      const result = getTrackedSuggestedRelationships({
        suggestedRelationships:
          chinook_data_set_suggested_relationships_from_api,
        fkConstraintRelationships: [
          {
            name: 'Employee_Employees',
            using: {
              foreign_key_constraint_on: {
                column: 'ReportsTo',
                table: {
                  name: 'Employee',
                  schema: 'public',
                },
              },
            },
            table: {
              name: 'Employee',
              schema: 'public',
            },
            type: 'array',
          },
        ],
      });
      expect(result).toStrictEqual([
        {
          name: 'Employee_Employees',
          fromTable: {
            name: 'Employee',
            schema: 'public',
          },
          toTable: {
            name: 'Employee',
            schema: 'public',
          },
          columnMapping: {
            EmployeeId: 'ReportsTo',
          },
          type: 'array',
        },
      ]);
    });

    it('[ARRAY] Genre_Tracks', () => {
      const result = getTrackedSuggestedRelationships({
        suggestedRelationships:
          chinook_data_set_suggested_relationships_from_api,
        fkConstraintRelationships: [
          {
            name: 'Genre_Tracks',
            using: {
              foreign_key_constraint_on: {
                column: 'GenreId',
                table: {
                  name: 'Track',
                  schema: 'public',
                },
              },
            },
            table: {
              name: 'Genre',
              schema: 'public',
            },
            type: 'array',
          },
        ],
      });
      expect(result).toStrictEqual([
        {
          name: 'Genre_Tracks',
          fromTable: {
            name: 'Genre',
            schema: 'public',
          },
          toTable: {
            name: 'Track',
            schema: 'public',
          },
          columnMapping: {
            GenreId: 'GenreId',
          },
          type: 'array',
        },
      ]);
    });

    it('[ARRAY] Invoice_InvoiceLines', () => {
      const result = getTrackedSuggestedRelationships({
        suggestedRelationships:
          chinook_data_set_suggested_relationships_from_api,
        fkConstraintRelationships: [
          {
            name: 'Invoice_InvoiceLines',
            using: {
              foreign_key_constraint_on: {
                column: 'InvoiceId',
                table: {
                  name: 'InvoiceLine',
                  schema: 'public',
                },
              },
            },
            table: {
              name: 'Invoice',
              schema: 'public',
            },
            type: 'array',
          },
        ],
      });
      expect(result).toStrictEqual([
        {
          name: 'Invoice_InvoiceLines',
          fromTable: {
            name: 'Invoice',
            schema: 'public',
          },
          toTable: {
            name: 'InvoiceLine',
            schema: 'public',
          },
          columnMapping: {
            InvoiceId: 'InvoiceId',
          },
          type: 'array',
        },
      ]);
    });

    it('[ARRAY] MediaType_Tracks', () => {
      const result = getTrackedSuggestedRelationships({
        suggestedRelationships:
          chinook_data_set_suggested_relationships_from_api,
        fkConstraintRelationships: [
          {
            name: 'MediaType_Tracks',
            using: {
              foreign_key_constraint_on: {
                column: 'MediaTypeId',
                table: {
                  name: 'Track',
                  schema: 'public',
                },
              },
            },
            table: {
              name: 'MediaType',
              schema: 'public',
            },
            type: 'array',
          },
        ],
      });
      expect(result).toStrictEqual([
        {
          name: 'MediaType_Tracks',
          fromTable: {
            name: 'MediaType',
            schema: 'public',
          },
          toTable: {
            name: 'Track',
            schema: 'public',
          },
          columnMapping: {
            MediaTypeId: 'MediaTypeId',
          },
          type: 'array',
        },
      ]);
    });

    it('[ARRAY] Playlist_PlaylistTracks', () => {
      const result = getTrackedSuggestedRelationships({
        suggestedRelationships:
          chinook_data_set_suggested_relationships_from_api,
        fkConstraintRelationships: [
          {
            name: 'Playlist_PlaylistTracks',
            using: {
              foreign_key_constraint_on: {
                column: 'PlaylistId',
                table: {
                  name: 'PlaylistTrack',
                  schema: 'public',
                },
              },
            },
            table: {
              name: 'Playlist',
              schema: 'public',
            },
            type: 'array',
          },
        ],
      });
      expect(result).toStrictEqual([
        {
          name: 'Playlist_PlaylistTracks',
          fromTable: {
            name: 'Playlist',
            schema: 'public',
          },
          toTable: {
            name: 'PlaylistTrack',
            schema: 'public',
          },
          columnMapping: {
            PlaylistId: 'PlaylistId',
          },
          type: 'array',
        },
      ]);
    });

    it('[ARRAY] Track_InvoiceLines', () => {
      const result = getTrackedSuggestedRelationships({
        suggestedRelationships:
          chinook_data_set_suggested_relationships_from_api,
        fkConstraintRelationships: [
          {
            name: 'Track_InvoiceLines',
            using: {
              foreign_key_constraint_on: {
                column: 'TrackId',
                table: {
                  name: 'InvoiceLine',
                  schema: 'public',
                },
              },
            },
            table: {
              name: 'Track',
              schema: 'public',
            },
            type: 'array',
          },
        ],
      });
      expect(result).toStrictEqual([
        {
          name: 'Track_InvoiceLines',
          fromTable: {
            name: 'Track',
            schema: 'public',
          },
          toTable: {
            name: 'InvoiceLine',
            schema: 'public',
          },
          columnMapping: {
            TrackId: 'TrackId',
          },
          type: 'array',
        },
      ]);
    });

    it('[ARRAY] Track_PlaylistTracks', () => {
      const result = getTrackedSuggestedRelationships({
        suggestedRelationships:
          chinook_data_set_suggested_relationships_from_api,
        fkConstraintRelationships: [
          {
            name: 'Track_PlaylistTracks',
            using: {
              foreign_key_constraint_on: {
                column: 'TrackId',
                table: {
                  name: 'PlaylistTrack',
                  schema: 'public',
                },
              },
            },
            table: {
              name: 'Track',
              schema: 'public',
            },
            type: 'array',
          },
        ],
      });
      expect(result).toStrictEqual([
        {
          name: 'Track_PlaylistTracks',
          fromTable: {
            name: 'Track',
            schema: 'public',
          },
          toTable: {
            name: 'PlaylistTrack',
            schema: 'public',
          },
          columnMapping: {
            TrackId: 'TrackId',
          },
          type: 'array',
        },
      ]);
    });
  });

  describe('works for composite fk relationships', () => {
    it('[OBJECT] files_student', () => {
      const result = getTrackedSuggestedRelationships({
        suggestedRelationships:
          mock_composite_fk_suggested_relationships_from_api,
        fkConstraintRelationships: [
          {
            name: 'files_student',
            using: {
              foreign_key_constraint_on: [
                'y_student_id',
                'y_batch_id',
                'y_dept_id',
              ],
            },
            table: {
              name: 'files',
              schema: 'public',
            },
            type: 'object',
          },
        ],
      });
      expect(result).toStrictEqual([
        {
          name: 'files_student',
          fromTable: {
            name: 'files',
            schema: 'public',
          },
          toTable: {
            name: 'student',
            schema: 'public',
          },
          columnMapping: {
            y_student_id: 'x_student_id',
            y_batch_id: 'x_batch_id',
            y_dept_id: 'x_dept_id',
          },
          type: 'object',
        },
      ]);
    });

    it('[ARRAY] student_files', () => {
      const result = getTrackedSuggestedRelationships({
        suggestedRelationships:
          mock_composite_fk_suggested_relationships_from_api,
        fkConstraintRelationships: [
          {
            name: 'student_files',
            using: {
              foreign_key_constraint_on: {
                columns: ['y_batch_id', 'y_dept_id', 'y_student_id'],
                table: {
                  name: 'files',
                  schema: 'public',
                },
              },
            },
            table: {
              name: 'student',
              schema: 'public',
            },
            type: 'array',
          },
        ],
      });
      expect(result).toStrictEqual([
        {
          name: 'student_files',
          fromTable: {
            name: 'student',
            schema: 'public',
          },
          toTable: {
            name: 'files',
            schema: 'public',
          },
          columnMapping: {
            x_dept_id: 'y_dept_id',
            x_batch_id: 'y_batch_id',
            x_student_id: 'y_student_id',
          },
          type: 'array',
        },
      ]);
    });
  });

  describe('works for other cases', () => {
    it('[OBJECT] author_author_details (remote column constraint)', () => {
      const result = getTrackedSuggestedRelationships({
        suggestedRelationships: [
          {
            type: 'object',
            from: {
              table: {
                schema: 'public',
                name: 'author_details',
              },
              columns: ['author_id'],
              constraint_name: 'author_details_author_id_fkey',
            },
            to: {
              table: {
                schema: 'public',
                name: 'author',
              },
              columns: ['author_id'],
            },
          },
          {
            type: 'object',
            from: {
              table: {
                schema: 'public',
                name: 'author',
              },
              columns: ['author_id'],
            },
            to: {
              table: {
                schema: 'public',
                name: 'author_details',
              },
              columns: ['author_id'],
              constraint_name: 'author_details_author_id_fkey',
            },
          },
        ],
        fkConstraintRelationships: [
          {
            name: 'author_author_details',
            using: {
              foreign_key_constraint_on: {
                table: {
                  schema: 'public',
                  name: 'author_details',
                },
                columns: ['author_id'],
              },
            },
            table: {
              name: 'author',
              schema: 'public',
            },
            type: 'object',
          },
        ],
      });
      expect(result).toStrictEqual([
        {
          name: 'author_author_details',
          fromTable: {
            name: 'author',
            schema: 'public',
          },
          toTable: {
            name: 'author_details',
            schema: 'public',
          },
          columnMapping: {
            author_id: 'author_id',
          },
          type: 'object',
        },
      ]);
    });
  });
});

describe('filterBySchema', () => {
  it('get only FK rels for tables in schema_1', () => {
    const allFkRelsFromMetadata: ReturnType<typeof getForeignKeyRelationships> =
      [
        {
          name: 'Album_Tracks',
          using: {
            foreign_key_constraint_on: {
              column: 'AlbumId',
              table: {
                name: 'Track',
                schema: 'public',
              },
            },
          },
          table: {
            name: 'Album',
            schema: 'schema_1',
          },
          type: 'array',
        },
        {
          name: 'Artist_Albums',
          using: {
            foreign_key_constraint_on: {
              column: 'ArtistId',
              table: {
                name: 'Album',
                schema: 'public',
              },
            },
          },
          table: {
            name: 'Artist',
            schema: 'schema_1',
          },
          type: 'array',
        },
        {
          name: 'Customer_Invoices',
          using: {
            foreign_key_constraint_on: {
              column: 'CustomerId',
              table: {
                name: 'Invoice',
                schema: 'public',
              },
            },
          },
          table: {
            name: 'Customer',
            schema: 'schema_1',
          },
          type: 'array',
        },
        {
          name: 'Employee_Customers',
          using: {
            foreign_key_constraint_on: {
              column: 'SupportRepId',
              table: {
                name: 'Customer',
                schema: 'public',
              },
            },
          },
          table: {
            name: 'Employee',
            schema: 'schema_2',
          },
          type: 'array',
        },
        {
          name: 'Employee_Employees',
          using: {
            foreign_key_constraint_on: {
              column: 'ReportsTo',
              table: {
                name: 'Employee',
                schema: 'public',
              },
            },
          },
          table: {
            name: 'Employee',
            schema: 'schema_2',
          },
          type: 'array',
        },
        {
          name: 'Genre_Tracks',
          using: {
            foreign_key_constraint_on: {
              column: 'GenreId',
              table: {
                name: 'Track',
                schema: 'public',
              },
            },
          },
          table: {
            name: 'Genre',
            schema: 'schema_1',
          },
          type: 'array',
        },
        {
          name: 'Invoice_InvoiceLines',
          using: {
            foreign_key_constraint_on: {
              column: 'InvoiceId',
              table: {
                name: 'InvoiceLine',
                schema: 'public',
              },
            },
          },
          table: {
            name: 'Invoice',
            schema: 'public',
          },
          type: 'array',
        },
        {
          name: 'MediaType_Tracks',
          using: {
            foreign_key_constraint_on: {
              column: 'MediaTypeId',
              table: {
                name: 'Track',
                schema: 'public',
              },
            },
          },
          table: {
            name: 'MediaType',
            schema: 'public',
          },
          type: 'array',
        },
        {
          name: 'Playlist_PlaylistTracks',
          using: {
            foreign_key_constraint_on: {
              column: 'PlaylistId',
              table: {
                name: 'PlaylistTrack',
                schema: 'public',
              },
            },
          },
          table: {
            name: 'Playlist',
            schema: 'public',
          },
          type: 'array',
        },
        {
          name: 'Track_InvoiceLines',
          using: {
            foreign_key_constraint_on: {
              column: 'TrackId',
              table: {
                name: 'InvoiceLine',
                schema: 'public',
              },
            },
          },
          table: {
            name: 'Track',
            schema: 'public',
          },
          type: 'array',
        },
        {
          name: 'Track_PlaylistTracks',
          using: {
            foreign_key_constraint_on: {
              column: 'TrackId',
              table: {
                name: 'PlaylistTrack',
                schema: 'public',
              },
            },
          },
          table: {
            name: 'Track',
            schema: 'public',
          },
          type: 'array',
        },
      ];
    const result = filterBySchema(allFkRelsFromMetadata, 'schema_1');
    expect(result).toStrictEqual([
      {
        name: 'Album_Tracks',
        using: {
          foreign_key_constraint_on: {
            column: 'AlbumId',
            table: {
              name: 'Track',
              schema: 'public',
            },
          },
        },
        table: {
          name: 'Album',
          schema: 'schema_1',
        },
        type: 'array',
      },
      {
        name: 'Artist_Albums',
        using: {
          foreign_key_constraint_on: {
            column: 'ArtistId',
            table: {
              name: 'Album',
              schema: 'public',
            },
          },
        },
        table: {
          name: 'Artist',
          schema: 'schema_1',
        },
        type: 'array',
      },
      {
        name: 'Customer_Invoices',
        using: {
          foreign_key_constraint_on: {
            column: 'CustomerId',
            table: {
              name: 'Invoice',
              schema: 'public',
            },
          },
        },
        table: {
          name: 'Customer',
          schema: 'schema_1',
        },
        type: 'array',
      },
      {
        name: 'Genre_Tracks',
        using: {
          foreign_key_constraint_on: {
            column: 'GenreId',
            table: {
              name: 'Track',
              schema: 'public',
            },
          },
        },
        table: {
          name: 'Genre',
          schema: 'schema_1',
        },
        type: 'array',
      },
    ]);
  });
  it('returns the result as is if the tables are gdc tables', () => {
    const allFkRelsFromMetadata: ReturnType<typeof getForeignKeyRelationships> =
      [
        {
          name: 'Invoice_InvoiceLines',
          using: {
            foreign_key_constraint_on: {
              column: 'InvoiceId',
              table: ['public', 'InvoiceLine'],
            },
          },
          table: ['public', 'Invoice'],
          type: 'array',
        },
        {
          name: 'InvoiceLine_Invoice',
          using: {
            foreign_key_constraint_on: 'InvoiceId',
          },
          table: ['public', 'InvoiceLine'],
          type: 'object',
        },
        {
          name: 'InvoiceLine_Track',
          using: {
            foreign_key_constraint_on: 'TrackId',
          },
          table: ['public', 'InvoiceLine'],
          type: 'object',
        },
        {
          name: 'Track_InvoiceLines',
          using: {
            foreign_key_constraint_on: {
              column: 'TrackId',
              table: ['public', 'InvoiceLine'],
            },
          },
          table: ['public', 'Track'],
          type: 'array',
        },
      ];
    const result = filterBySchema(allFkRelsFromMetadata, 'public');
    expect(result).toStrictEqual(allFkRelsFromMetadata);
  });
});
