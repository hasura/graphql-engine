export const mocksTrackedArrayRelationship = {
  metadata: {
    resource_version: 136,
    metadata: {
      version: 3,
      sources: [
        {
          name: 'aPostgres',
          kind: 'postgres',
          tables: [
            {
              table: { name: 'Album', schema: 'public' },
              array_relationships: [
                {
                  name: 'albumAlbumCovers',
                  using: {
                    foreign_key_constraint_on: {
                      column: 'AlbumId',
                      table: { name: 'AlbumCovers', schema: 'public' },
                    },
                  },
                },
                {
                  name: 'albumTracks',
                  using: {
                    foreign_key_constraint_on: {
                      column: 'AlbumId',
                      table: { name: 'Track', schema: 'public' },
                    },
                  },
                },
              ],
              remote_relationships: [
                {
                  definition: {
                    to_source: {
                      field_mapping: {
                        ArtistId: 'ArtistId',
                      },
                      relationship_type: 'object',
                      source: 'aMySQL',
                      table: {
                        name: 'Artist',
                        schema: 'dbo',
                      },
                    },
                  },
                  name: 'albumArtistRemote',
                },
              ],
            },
            { table: { name: 'AlbumCovers', schema: 'public' } },
            {
              table: { name: 'Artist', schema: 'public' },
              array_relationships: [
                {
                  name: 'artistAlbums',
                  using: {
                    foreign_key_constraint_on: {
                      column: 'ArtistId',
                      table: { name: 'Album', schema: 'public' },
                    },
                  },
                },
              ],
            },
            { table: { name: 'Customer', schema: 'public' } },
            { table: { name: 'Employee', schema: 'public' } },
            { table: { name: 'Genre', schema: 'public' } },
            { table: { name: 'Invoice', schema: 'public' } },
            { table: { name: 'InvoiceLine', schema: 'public' } },
            { table: { name: 'MediaType', schema: 'public' } },
            { table: { name: 'Playlist', schema: 'public' } },
            { table: { name: 'PlaylistTrack', schema: 'public' } },
            { table: { name: 'Track', schema: 'public' } },
          ],
          native_queries: [
            {
              arguments: {},
              code: 'select "Title" as name from "Album"',
              returns: 'AlbumName',
              root_field_name: 'asdasd',
            },
          ],
          logical_models: [
            {
              fields: [
                { name: 'name', type: { nullable: false, scalar: 'varchar' } },
              ],
              name: 'AlbumName',
              select_permissions: [
                { permission: { columns: ['name'], filter: {} }, role: 'gino' },
                { permission: { columns: ['name'], filter: {} }, role: 'user' },
              ],
            },
            {
              fields: [
                { name: 'Title', type: { nullable: false, scalar: 'varchar' } },
              ],
              name: 'AlbumTitle',
            },
          ],
          configuration: {
            connection_info: {
              database_url: 'postgres://postgres:pass@postgres:5432/chinook',
              isolation_level: 'read-committed',
              use_prepared_statements: false,
            },
          },
          customization: { naming_convention: 'graphql-default' },
        },
        {
          name: 'aMySQL',
          kind: 'mysql8',
          tables: [
            {
              table: ['Artist'],
            },
          ],
          configuration: {
            template: null,
            timeout: null,
            value: {
              fully_qualify_all_names: false,
              jdbc_url:
                'jdbc:mysql://mysql:3306/Chinook?allowMultiQueries=true&user=root&password=pass',
            },
          },
        },
      ],
    },
  },
  suggestedRelationshipsOmitTracked: {
    relationships: [
      {
        type: 'object',
        from: {
          table: { schema: 'public', name: 'Album' },
          columns: ['ArtistId'],
          constraint_name: 'FK_AlbumArtistId',
        },
        to: {
          table: { schema: 'public', name: 'Artist' },
          columns: ['ArtistId'],
        },
      },
      {
        type: 'object',
        from: {
          table: { schema: 'public', name: 'Track' },
          columns: ['AlbumId'],
          constraint_name: 'FK_TrackAlbumId',
        },
        to: {
          table: { schema: 'public', name: 'Album' },
          columns: ['AlbumId'],
        },
      },
      /**
       * This mock includes an extra table not included in the original Chinook database.
       * The table AlbumCovers has been added to show two Array relationships in the Suggested Relationships view.
       *
       * Album -> AlbumCovers
       * Album -> Track
       */
      {
        type: 'object',
        from: {
          table: { schema: 'public', name: 'AlbumCovers' },
          columns: ['AlbumId'],
          constraint_name: 'AlbumCovers_AlbumId_fkey',
        },
        to: {
          table: { schema: 'public', name: 'Album' },
          columns: ['AlbumId'],
        },
      },
    ],
  },
  suggestedRelationships: {
    relationships: [
      {
        type: 'object',
        from: {
          table: { schema: 'public', name: 'Invoice' },
          columns: ['CustomerId'],
          constraint_name: 'FK_InvoiceCustomerId',
        },
        to: {
          table: { schema: 'public', name: 'Customer' },
          columns: ['CustomerId'],
        },
      },
      {
        type: 'array',
        from: {
          table: { schema: 'public', name: 'Customer' },
          columns: ['CustomerId'],
        },
        to: {
          table: { schema: 'public', name: 'Invoice' },
          columns: ['CustomerId'],
          constraint_name: 'FK_InvoiceCustomerId',
        },
      },
      {
        type: 'object',
        from: {
          table: { schema: 'public', name: 'InvoiceLine' },
          columns: ['TrackId'],
          constraint_name: 'FK_InvoiceLineTrackId',
        },
        to: {
          table: { schema: 'public', name: 'Track' },
          columns: ['TrackId'],
        },
      },
      {
        type: 'array',
        from: {
          table: { schema: 'public', name: 'Track' },
          columns: ['TrackId'],
        },
        to: {
          table: { schema: 'public', name: 'InvoiceLine' },
          columns: ['TrackId'],
          constraint_name: 'FK_InvoiceLineTrackId',
        },
      },
      {
        type: 'object',
        from: {
          table: { schema: 'public', name: 'InvoiceLine' },
          columns: ['InvoiceId'],
          constraint_name: 'FK_InvoiceLineInvoiceId',
        },
        to: {
          table: { schema: 'public', name: 'Invoice' },
          columns: ['InvoiceId'],
        },
      },
      {
        type: 'array',
        from: {
          table: { schema: 'public', name: 'Invoice' },
          columns: ['InvoiceId'],
        },
        to: {
          table: { schema: 'public', name: 'InvoiceLine' },
          columns: ['InvoiceId'],
          constraint_name: 'FK_InvoiceLineInvoiceId',
        },
      },
      {
        type: 'object',
        from: {
          table: { schema: 'public', name: 'Employee' },
          columns: ['ReportsTo'],
          constraint_name: 'FK_EmployeeReportsTo',
        },
        to: {
          table: { schema: 'public', name: 'Employee' },
          columns: ['EmployeeId'],
        },
      },
      {
        type: 'array',
        from: {
          table: { schema: 'public', name: 'Employee' },
          columns: ['EmployeeId'],
        },
        to: {
          table: { schema: 'public', name: 'Employee' },
          columns: ['ReportsTo'],
          constraint_name: 'FK_EmployeeReportsTo',
        },
      },
      {
        type: 'object',
        from: {
          table: { schema: 'public', name: 'Album' },
          columns: ['ArtistId'],
          constraint_name: 'FK_AlbumArtistId',
        },
        to: {
          table: { schema: 'public', name: 'Artist' },
          columns: ['ArtistId'],
        },
      },
      {
        type: 'array',
        from: {
          table: { schema: 'public', name: 'Artist' },
          columns: ['ArtistId'],
        },
        to: {
          table: { schema: 'public', name: 'Album' },
          columns: ['ArtistId'],
          constraint_name: 'FK_AlbumArtistId',
        },
      },
      {
        type: 'object',
        from: {
          table: { schema: 'public', name: 'Customer' },
          columns: ['SupportRepId'],
          constraint_name: 'FK_CustomerSupportRepId',
        },
        to: {
          table: { schema: 'public', name: 'Employee' },
          columns: ['EmployeeId'],
        },
      },
      {
        type: 'array',
        from: {
          table: { schema: 'public', name: 'Employee' },
          columns: ['EmployeeId'],
        },
        to: {
          table: { schema: 'public', name: 'Customer' },
          columns: ['SupportRepId'],
          constraint_name: 'FK_CustomerSupportRepId',
        },
      },
      {
        type: 'object',
        from: {
          table: { schema: 'public', name: 'PlaylistTrack' },
          columns: ['PlaylistId'],
          constraint_name: 'FK_PlaylistTrackPlaylistId',
        },
        to: {
          table: { schema: 'public', name: 'Playlist' },
          columns: ['PlaylistId'],
        },
      },
      {
        type: 'array',
        from: {
          table: { schema: 'public', name: 'Playlist' },
          columns: ['PlaylistId'],
        },
        to: {
          table: { schema: 'public', name: 'PlaylistTrack' },
          columns: ['PlaylistId'],
          constraint_name: 'FK_PlaylistTrackPlaylistId',
        },
      },
      {
        type: 'object',
        from: {
          table: { schema: 'public', name: 'PlaylistTrack' },
          columns: ['TrackId'],
          constraint_name: 'FK_PlaylistTrackTrackId',
        },
        to: {
          table: { schema: 'public', name: 'Track' },
          columns: ['TrackId'],
        },
      },
      {
        type: 'array',
        from: {
          table: { schema: 'public', name: 'Track' },
          columns: ['TrackId'],
        },
        to: {
          table: { schema: 'public', name: 'PlaylistTrack' },
          columns: ['TrackId'],
          constraint_name: 'FK_PlaylistTrackTrackId',
        },
      },
      {
        type: 'object',
        from: {
          table: { schema: 'public', name: 'Track' },
          columns: ['AlbumId'],
          constraint_name: 'FK_TrackAlbumId',
        },
        to: {
          table: { schema: 'public', name: 'Album' },
          columns: ['AlbumId'],
        },
      },
      {
        type: 'array',
        from: {
          table: { schema: 'public', name: 'Album' },
          columns: ['AlbumId'],
        },
        to: {
          table: { schema: 'public', name: 'Track' },
          columns: ['AlbumId'],
          constraint_name: 'FK_TrackAlbumId',
        },
      },
      {
        type: 'object',
        from: {
          table: { schema: 'public', name: 'Track' },
          columns: ['GenreId'],
          constraint_name: 'FK_TrackGenreId',
        },
        to: {
          table: { schema: 'public', name: 'Genre' },
          columns: ['GenreId'],
        },
      },
      {
        type: 'array',
        from: {
          table: { schema: 'public', name: 'Genre' },
          columns: ['GenreId'],
        },
        to: {
          table: { schema: 'public', name: 'Track' },
          columns: ['GenreId'],
          constraint_name: 'FK_TrackGenreId',
        },
      },
      {
        type: 'object',
        from: {
          table: { schema: 'public', name: 'Track' },
          columns: ['MediaTypeId'],
          constraint_name: 'FK_TrackMediaTypeId',
        },
        to: {
          table: { schema: 'public', name: 'MediaType' },
          columns: ['MediaTypeId'],
        },
      },
      {
        type: 'array',
        from: {
          table: { schema: 'public', name: 'MediaType' },
          columns: ['MediaTypeId'],
        },
        to: {
          table: { schema: 'public', name: 'Track' },
          columns: ['MediaTypeId'],
          constraint_name: 'FK_TrackMediaTypeId',
        },
      },
      {
        type: 'object',
        from: {
          table: { schema: 'public', name: 'AlbumCovers' },
          columns: ['AlbumId'],
          constraint_name: 'AlbumCovers_AlbumId_fkey',
        },
        to: {
          table: { schema: 'public', name: 'Album' },
          columns: ['AlbumId'],
        },
      },
      {
        type: 'array',
        from: {
          table: { schema: 'public', name: 'Album' },
          columns: ['AlbumId'],
        },
        to: {
          table: { schema: 'public', name: 'AlbumCovers' },
          columns: ['AlbumId'],
          constraint_name: 'AlbumCovers_AlbumId_fkey',
        },
      },
    ],
  },
  listSourceKinds: {
    sources: [
      { available: true, builtin: true, display_name: 'pg', kind: 'pg' },
      {
        available: true,
        builtin: true,
        display_name: 'citus',
        kind: 'citus',
      },
      {
        available: true,
        builtin: true,
        display_name: 'cockroach',
        kind: 'cockroach',
      },
      {
        available: true,
        builtin: true,
        display_name: 'mssql',
        kind: 'mssql',
      },
      {
        available: true,
        builtin: true,
        display_name: 'bigquery',
        kind: 'bigquery',
      },
      {
        available: true,
        builtin: false,
        display_name: 'Snowflake',
        kind: 'snowflake',
        release_name: 'Beta',
      },
    ],
  },
};
