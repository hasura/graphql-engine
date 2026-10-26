import { SuggestedRelationship } from './types';
export const chinook_data_set_suggested_relationships_from_api: SuggestedRelationship[] =
  [
    {
      type: 'object',
      from: {
        table: {
          schema: 'public',
          name: 'Album',
        },
        columns: ['ArtistId'],
        constraint_name: 'FK_AlbumArtistId',
      },
      to: {
        table: {
          schema: 'public',
          name: 'Artist',
        },
        columns: ['ArtistId'],
      },
    },
    {
      type: 'array',
      from: {
        table: {
          schema: 'public',
          name: 'Artist',
        },
        columns: ['ArtistId'],
      },
      to: {
        table: {
          schema: 'public',
          name: 'Album',
        },
        columns: ['ArtistId'],
        constraint_name: 'FK_AlbumArtistId',
      },
    },
    {
      type: 'object',
      from: {
        table: {
          schema: 'public',
          name: 'Employee',
        },
        columns: ['ReportsTo'],
        constraint_name: 'FK_EmployeeReportsTo',
      },
      to: {
        table: {
          schema: 'public',
          name: 'Employee',
        },
        columns: ['EmployeeId'],
      },
    },
    {
      type: 'array',
      from: {
        table: {
          schema: 'public',
          name: 'Employee',
        },
        columns: ['EmployeeId'],
      },
      to: {
        table: {
          schema: 'public',
          name: 'Employee',
        },
        columns: ['ReportsTo'],
        constraint_name: 'FK_EmployeeReportsTo',
      },
    },
    {
      type: 'object',
      from: {
        table: {
          schema: 'public',
          name: 'InvoiceLine',
        },
        columns: ['InvoiceId'],
        constraint_name: 'FK_InvoiceLineInvoiceId',
      },
      to: {
        table: {
          schema: 'public',
          name: 'Invoice',
        },
        columns: ['InvoiceId'],
      },
    },
    {
      type: 'array',
      from: {
        table: {
          schema: 'public',
          name: 'Invoice',
        },
        columns: ['InvoiceId'],
      },
      to: {
        table: {
          schema: 'public',
          name: 'InvoiceLine',
        },
        columns: ['InvoiceId'],
        constraint_name: 'FK_InvoiceLineInvoiceId',
      },
    },
    {
      type: 'object',
      from: {
        table: {
          schema: 'public',
          name: 'InvoiceLine',
        },
        columns: ['TrackId'],
        constraint_name: 'FK_InvoiceLineTrackId',
      },
      to: {
        table: {
          schema: 'public',
          name: 'Track',
        },
        columns: ['TrackId'],
      },
    },
    {
      type: 'array',
      from: {
        table: {
          schema: 'public',
          name: 'Track',
        },
        columns: ['TrackId'],
      },
      to: {
        table: {
          schema: 'public',
          name: 'InvoiceLine',
        },
        columns: ['TrackId'],
        constraint_name: 'FK_InvoiceLineTrackId',
      },
    },
    {
      type: 'object',
      from: {
        table: {
          schema: 'public',
          name: 'PlaylistTrack',
        },
        columns: ['TrackId'],
        constraint_name: 'FK_PlaylistTrackTrackId',
      },
      to: {
        table: {
          schema: 'public',
          name: 'Track',
        },
        columns: ['TrackId'],
      },
    },
    {
      type: 'array',
      from: {
        table: {
          schema: 'public',
          name: 'Track',
        },
        columns: ['TrackId'],
      },
      to: {
        table: {
          schema: 'public',
          name: 'PlaylistTrack',
        },
        columns: ['TrackId'],
        constraint_name: 'FK_PlaylistTrackTrackId',
      },
    },
    {
      type: 'object',
      from: {
        table: {
          schema: 'public',
          name: 'PlaylistTrack',
        },
        columns: ['PlaylistId'],
        constraint_name: 'FK_PlaylistTrackPlaylistId',
      },
      to: {
        table: {
          schema: 'public',
          name: 'Playlist',
        },
        columns: ['PlaylistId'],
      },
    },
    {
      type: 'array',
      from: {
        table: {
          schema: 'public',
          name: 'Playlist',
        },
        columns: ['PlaylistId'],
      },
      to: {
        table: {
          schema: 'public',
          name: 'PlaylistTrack',
        },
        columns: ['PlaylistId'],
        constraint_name: 'FK_PlaylistTrackPlaylistId',
      },
    },
    {
      type: 'object',
      from: {
        table: {
          schema: 'public',
          name: 'Customer',
        },
        columns: ['SupportRepId'],
        constraint_name: 'FK_CustomerSupportRepId',
      },
      to: {
        table: {
          schema: 'public',
          name: 'Employee',
        },
        columns: ['EmployeeId'],
      },
    },
    {
      type: 'array',
      from: {
        table: {
          schema: 'public',
          name: 'Employee',
        },
        columns: ['EmployeeId'],
      },
      to: {
        table: {
          schema: 'public',
          name: 'Customer',
        },
        columns: ['SupportRepId'],
        constraint_name: 'FK_CustomerSupportRepId',
      },
    },
    {
      type: 'object',
      from: {
        table: {
          schema: 'public',
          name: 'Track',
        },
        columns: ['MediaTypeId'],
        constraint_name: 'FK_TrackMediaTypeId',
      },
      to: {
        table: {
          schema: 'public',
          name: 'MediaType',
        },
        columns: ['MediaTypeId'],
      },
    },
    {
      type: 'array',
      from: {
        table: {
          schema: 'public',
          name: 'MediaType',
        },
        columns: ['MediaTypeId'],
      },
      to: {
        table: {
          schema: 'public',
          name: 'Track',
        },
        columns: ['MediaTypeId'],
        constraint_name: 'FK_TrackMediaTypeId',
      },
    },
    {
      type: 'object',
      from: {
        table: {
          schema: 'public',
          name: 'Track',
        },
        columns: ['AlbumId'],
        constraint_name: 'FK_TrackAlbumId',
      },
      to: {
        table: {
          schema: 'public',
          name: 'Album',
        },
        columns: ['AlbumId'],
      },
    },
    {
      type: 'array',
      from: {
        table: {
          schema: 'public',
          name: 'Album',
        },
        columns: ['AlbumId'],
      },
      to: {
        table: {
          schema: 'public',
          name: 'Track',
        },
        columns: ['AlbumId'],
        constraint_name: 'FK_TrackAlbumId',
      },
    },
    {
      type: 'object',
      from: {
        table: {
          schema: 'public',
          name: 'Track',
        },
        columns: ['GenreId'],
        constraint_name: 'FK_TrackGenreId',
      },
      to: {
        table: {
          schema: 'public',
          name: 'Genre',
        },
        columns: ['GenreId'],
      },
    },
    {
      type: 'array',
      from: {
        table: {
          schema: 'public',
          name: 'Genre',
        },
        columns: ['GenreId'],
      },
      to: {
        table: {
          schema: 'public',
          name: 'Track',
        },
        columns: ['GenreId'],
        constraint_name: 'FK_TrackGenreId',
      },
    },
    {
      type: 'object',
      from: {
        table: {
          schema: 'public',
          name: 'Invoice',
        },
        columns: ['CustomerId'],
        constraint_name: 'FK_InvoiceCustomerId',
      },
      to: {
        table: {
          schema: 'public',
          name: 'Customer',
        },
        columns: ['CustomerId'],
      },
    },
    {
      type: 'array',
      from: {
        table: {
          schema: 'public',
          name: 'Customer',
        },
        columns: ['CustomerId'],
      },
      to: {
        table: {
          schema: 'public',
          name: 'Invoice',
        },
        columns: ['CustomerId'],
        constraint_name: 'FK_InvoiceCustomerId',
      },
    },
  ];

export const mock_composite_fk_suggested_relationships_from_api: SuggestedRelationship[] =
  [
    {
      type: 'object',
      from: {
        table: {
          schema: 'public',
          name: 'files',
        },
        columns: ['y_student_id', 'y_batch_id', 'y_dept_id'],
        constraint_name: 'fk_student_files',
      },
      to: {
        table: {
          schema: 'public',
          name: 'student',
        },
        columns: ['x_student_id', 'x_batch_id', 'x_dept_id'],
      },
    },
    {
      type: 'array',
      from: {
        table: {
          schema: 'public',
          name: 'student',
        },
        columns: ['x_student_id', 'x_batch_id', 'x_dept_id'],
      },
      to: {
        table: {
          schema: 'public',
          name: 'files',
        },
        columns: ['y_student_id', 'y_batch_id', 'y_dept_id'],
        constraint_name: 'fk_student_files',
      },
    },
  ];
