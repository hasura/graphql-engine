import { Tables } from '../../../types';

export const tables = [
  {
    table: ['Album'],
    dataSource: {
      name: 'MySQ',
      kind: 'mysql8',
      tables: [
        {
          table: ['Album'],
          object_relationships: [
            {
              name: 'Artist',
              using: {
                foreign_key_constraint_on: 'ArtistId',
              },
            },
          ],
          array_relationships: [
            {
              name: 'Tracks',
              using: {
                foreign_key_constraint_on: {
                  column: 'AlbumId',
                  table: ['Track'],
                },
              },
            },
          ],
          select_permissions: [
            {
              role: 'a',
              permission: {
                columns: ['AlbumId', 'Title', 'ArtistId'],
                filter: {},
              },
              comment: '',
            },
            {
              role: 'v',
              permission: {
                columns: [],
                filter: {
                  Artist: {
                    Albums: {
                      Name: {
                        _eq: '',
                      },
                    },
                  },
                },
              },
              comment: '',
            },
          ],
        },
        {
          table: ['Artist'],
          array_relationships: [
            {
              name: 'Albums',
              using: {
                foreign_key_constraint_on: {
                  column: 'ArtistId',
                  table: ['Album'],
                },
              },
            },
          ],
        },
        {
          table: ['Customer'],
          object_relationships: [
            {
              name: 'Employee',
              using: {
                foreign_key_constraint_on: 'SupportRepId',
              },
            },
          ],
          array_relationships: [
            {
              name: 'Invoices',
              using: {
                foreign_key_constraint_on: {
                  column: 'CustomerId',
                  table: ['Invoice'],
                },
              },
            },
          ],
        },
        {
          table: ['Employee'],
          object_relationships: [
            {
              name: 'Employee',
              using: {
                foreign_key_constraint_on: 'ReportsTo',
              },
            },
          ],
          array_relationships: [
            {
              name: 'Customers',
              using: {
                foreign_key_constraint_on: {
                  column: 'SupportRepId',
                  table: ['Customer'],
                },
              },
            },
            {
              name: 'Employees',
              using: {
                foreign_key_constraint_on: {
                  column: 'ReportsTo',
                  table: ['Employee'],
                },
              },
            },
          ],
        },
        {
          table: ['Genre'],
          array_relationships: [
            {
              name: 'Tracks',
              using: {
                foreign_key_constraint_on: {
                  column: 'GenreId',
                  table: ['Track'],
                },
              },
            },
          ],
        },
        {
          table: ['Invoice'],
          object_relationships: [
            {
              name: 'Customer',
              using: {
                foreign_key_constraint_on: 'CustomerId',
              },
            },
          ],
          array_relationships: [
            {
              name: 'InvoiceLines',
              using: {
                foreign_key_constraint_on: {
                  column: 'InvoiceId',
                  table: ['InvoiceLine'],
                },
              },
            },
          ],
        },
        {
          table: ['InvoiceLine'],
          object_relationships: [
            {
              name: 'Invoice',
              using: {
                foreign_key_constraint_on: 'InvoiceId',
              },
            },
            {
              name: 'Track',
              using: {
                foreign_key_constraint_on: 'TrackId',
              },
            },
          ],
        },
        {
          table: ['MediaType'],
          array_relationships: [
            {
              name: 'Tracks',
              using: {
                foreign_key_constraint_on: {
                  column: 'MediaTypeId',
                  table: ['Track'],
                },
              },
            },
          ],
        },
        {
          table: ['Playlist'],
          array_relationships: [
            {
              name: 'PlaylistTracks',
              using: {
                foreign_key_constraint_on: {
                  column: 'PlaylistId',
                  table: ['PlaylistTrack'],
                },
              },
            },
          ],
        },
        {
          table: ['PlaylistTrack'],
          object_relationships: [
            {
              name: 'Playlist',
              using: {
                foreign_key_constraint_on: 'PlaylistId',
              },
            },
            {
              name: 'Track',
              using: {
                foreign_key_constraint_on: 'TrackId',
              },
            },
          ],
        },
        {
          table: ['Track'],
          object_relationships: [
            {
              name: 'Album',
              using: {
                foreign_key_constraint_on: 'AlbumId',
              },
            },
            {
              name: 'Genre',
              using: {
                foreign_key_constraint_on: 'GenreId',
              },
            },
            {
              name: 'MediaType',
              using: {
                foreign_key_constraint_on: 'MediaTypeId',
              },
            },
          ],
          array_relationships: [
            {
              name: 'InvoiceLines',
              using: {
                foreign_key_constraint_on: {
                  column: 'TrackId',
                  table: ['InvoiceLine'],
                },
              },
            },
            {
              name: 'PlaylistTracks',
              using: {
                foreign_key_constraint_on: {
                  column: 'TrackId',
                  table: ['PlaylistTrack'],
                },
              },
            },
          ],
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
    relationships: [
      {
        name: 'Tracks',
        fromSource: 'MySQ',
        fromTable: ['Album'],
        relationshipType: 'Array',
        type: 'localRelationship',
        definition: {
          toTable: ['Track'],
          toColumns: ['AlbumId'],
          fromTable: ['Album'],
          fromColumns: ['AlbumId'],
          mapping: {
            AlbumId: 'AlbumId',
          },
        },
      },
      {
        name: 'Artist',
        fromSource: 'MySQ',
        fromTable: ['Album'],
        relationshipType: 'Object',
        type: 'localRelationship',
        definition: {
          toTable: ['Artist'],
          toColumns: ['ArtistId'],
          fromTable: ['Album'],
          fromColumns: ['ArtistId'],
          mapping: {
            ArtistId: 'ArtistId',
          },
        },
      },
    ],
    columns: [
      {
        name: 'AlbumId',
        dataType: 'string',
        consoleDataType: 'string',
        nullable: false,
        isPrimaryKey: true,
        graphQLProperties: {
          name: 'AlbumId',
          scalarType: 'Int',
          graphQLType: 'Int!',
        },
      },
      {
        name: 'Title',
        dataType: 'string',
        consoleDataType: 'string',
        nullable: false,
        isPrimaryKey: false,
        graphQLProperties: {
          name: 'Title',
          scalarType: 'String',
          graphQLType: 'String!',
        },
      },
      {
        name: 'ArtistId',
        dataType: 'string',
        consoleDataType: 'string',
        nullable: false,
        isPrimaryKey: false,
        graphQLProperties: {
          name: 'ArtistId',
          scalarType: 'Int',
          graphQLType: 'Int!',
        },
      },
    ],
  },
  {
    table: ['Artist'],
    dataSource: {
      name: 'MySQ',
      kind: 'mysql8',
      tables: [
        {
          table: ['Album'],
          object_relationships: [
            {
              name: 'Artist',
              using: {
                foreign_key_constraint_on: 'ArtistId',
              },
            },
          ],
          array_relationships: [
            {
              name: 'Tracks',
              using: {
                foreign_key_constraint_on: {
                  column: 'AlbumId',
                  table: ['Track'],
                },
              },
            },
          ],
          select_permissions: [
            {
              role: 'a',
              permission: {
                columns: ['AlbumId', 'Title', 'ArtistId'],
                filter: {},
              },
              comment: '',
            },
            {
              role: 'v',
              permission: {
                columns: [],
                filter: {
                  Artist: {
                    Albums: {
                      Name: {
                        _eq: '',
                      },
                    },
                  },
                },
              },
              comment: '',
            },
          ],
        },
        {
          table: ['Artist'],
          array_relationships: [
            {
              name: 'Albums',
              using: {
                foreign_key_constraint_on: {
                  column: 'ArtistId',
                  table: ['Album'],
                },
              },
            },
          ],
        },
        {
          table: ['Customer'],
          object_relationships: [
            {
              name: 'Employee',
              using: {
                foreign_key_constraint_on: 'SupportRepId',
              },
            },
          ],
          array_relationships: [
            {
              name: 'Invoices',
              using: {
                foreign_key_constraint_on: {
                  column: 'CustomerId',
                  table: ['Invoice'],
                },
              },
            },
          ],
        },
        {
          table: ['Employee'],
          object_relationships: [
            {
              name: 'Employee',
              using: {
                foreign_key_constraint_on: 'ReportsTo',
              },
            },
          ],
          array_relationships: [
            {
              name: 'Customers',
              using: {
                foreign_key_constraint_on: {
                  column: 'SupportRepId',
                  table: ['Customer'],
                },
              },
            },
            {
              name: 'Employees',
              using: {
                foreign_key_constraint_on: {
                  column: 'ReportsTo',
                  table: ['Employee'],
                },
              },
            },
          ],
        },
        {
          table: ['Genre'],
          array_relationships: [
            {
              name: 'Tracks',
              using: {
                foreign_key_constraint_on: {
                  column: 'GenreId',
                  table: ['Track'],
                },
              },
            },
          ],
        },
        {
          table: ['Invoice'],
          object_relationships: [
            {
              name: 'Customer',
              using: {
                foreign_key_constraint_on: 'CustomerId',
              },
            },
          ],
          array_relationships: [
            {
              name: 'InvoiceLines',
              using: {
                foreign_key_constraint_on: {
                  column: 'InvoiceId',
                  table: ['InvoiceLine'],
                },
              },
            },
          ],
        },
        {
          table: ['InvoiceLine'],
          object_relationships: [
            {
              name: 'Invoice',
              using: {
                foreign_key_constraint_on: 'InvoiceId',
              },
            },
            {
              name: 'Track',
              using: {
                foreign_key_constraint_on: 'TrackId',
              },
            },
          ],
        },
        {
          table: ['MediaType'],
          array_relationships: [
            {
              name: 'Tracks',
              using: {
                foreign_key_constraint_on: {
                  column: 'MediaTypeId',
                  table: ['Track'],
                },
              },
            },
          ],
        },
        {
          table: ['Playlist'],
          array_relationships: [
            {
              name: 'PlaylistTracks',
              using: {
                foreign_key_constraint_on: {
                  column: 'PlaylistId',
                  table: ['PlaylistTrack'],
                },
              },
            },
          ],
        },
        {
          table: ['PlaylistTrack'],
          object_relationships: [
            {
              name: 'Playlist',
              using: {
                foreign_key_constraint_on: 'PlaylistId',
              },
            },
            {
              name: 'Track',
              using: {
                foreign_key_constraint_on: 'TrackId',
              },
            },
          ],
        },
        {
          table: ['Track'],
          object_relationships: [
            {
              name: 'Album',
              using: {
                foreign_key_constraint_on: 'AlbumId',
              },
            },
            {
              name: 'Genre',
              using: {
                foreign_key_constraint_on: 'GenreId',
              },
            },
            {
              name: 'MediaType',
              using: {
                foreign_key_constraint_on: 'MediaTypeId',
              },
            },
          ],
          array_relationships: [
            {
              name: 'InvoiceLines',
              using: {
                foreign_key_constraint_on: {
                  column: 'TrackId',
                  table: ['InvoiceLine'],
                },
              },
            },
            {
              name: 'PlaylistTracks',
              using: {
                foreign_key_constraint_on: {
                  column: 'TrackId',
                  table: ['PlaylistTrack'],
                },
              },
            },
          ],
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
    relationships: [
      {
        name: 'Albums',
        fromSource: 'MySQ',
        fromTable: ['Artist'],
        relationshipType: 'Array',
        type: 'localRelationship',
        definition: {
          toTable: ['Album'],
          toColumns: ['ArtistId'],
          fromTable: ['Artist'],
          fromColumns: ['ArtistId'],
          mapping: {
            ArtistId: 'ArtistId',
          },
        },
      },
    ],
    columns: [
      {
        name: 'ArtistId',
        dataType: 'string',
        consoleDataType: 'string',
        nullable: false,
        isPrimaryKey: true,
        graphQLProperties: {
          name: 'ArtistId',
          scalarType: 'Int',
          graphQLType: 'Int!',
        },
      },
      {
        name: 'Name',
        dataType: 'string',
        consoleDataType: 'string',
        nullable: true,
        isPrimaryKey: false,
        graphQLProperties: {
          name: 'Name',
          scalarType: 'String',
          graphQLType: 'String',
        },
      },
    ],
  },
  {
    table: ['Customer'],
    dataSource: {
      name: 'MySQ',
      kind: 'mysql8',
      tables: [
        {
          table: ['Album'],
          object_relationships: [
            {
              name: 'Artist',
              using: {
                foreign_key_constraint_on: 'ArtistId',
              },
            },
          ],
          array_relationships: [
            {
              name: 'Tracks',
              using: {
                foreign_key_constraint_on: {
                  column: 'AlbumId',
                  table: ['Track'],
                },
              },
            },
          ],
          select_permissions: [
            {
              role: 'a',
              permission: {
                columns: ['AlbumId', 'Title', 'ArtistId'],
                filter: {},
              },
              comment: '',
            },
            {
              role: 'v',
              permission: {
                columns: [],
                filter: {
                  Artist: {
                    Albums: {
                      Name: {
                        _eq: '',
                      },
                    },
                  },
                },
              },
              comment: '',
            },
          ],
        },
        {
          table: ['Artist'],
          array_relationships: [
            {
              name: 'Albums',
              using: {
                foreign_key_constraint_on: {
                  column: 'ArtistId',
                  table: ['Album'],
                },
              },
            },
          ],
        },
        {
          table: ['Customer'],
          object_relationships: [
            {
              name: 'Employee',
              using: {
                foreign_key_constraint_on: 'SupportRepId',
              },
            },
          ],
          array_relationships: [
            {
              name: 'Invoices',
              using: {
                foreign_key_constraint_on: {
                  column: 'CustomerId',
                  table: ['Invoice'],
                },
              },
            },
          ],
        },
        {
          table: ['Employee'],
          object_relationships: [
            {
              name: 'Employee',
              using: {
                foreign_key_constraint_on: 'ReportsTo',
              },
            },
          ],
          array_relationships: [
            {
              name: 'Customers',
              using: {
                foreign_key_constraint_on: {
                  column: 'SupportRepId',
                  table: ['Customer'],
                },
              },
            },
            {
              name: 'Employees',
              using: {
                foreign_key_constraint_on: {
                  column: 'ReportsTo',
                  table: ['Employee'],
                },
              },
            },
          ],
        },
        {
          table: ['Genre'],
          array_relationships: [
            {
              name: 'Tracks',
              using: {
                foreign_key_constraint_on: {
                  column: 'GenreId',
                  table: ['Track'],
                },
              },
            },
          ],
        },
        {
          table: ['Invoice'],
          object_relationships: [
            {
              name: 'Customer',
              using: {
                foreign_key_constraint_on: 'CustomerId',
              },
            },
          ],
          array_relationships: [
            {
              name: 'InvoiceLines',
              using: {
                foreign_key_constraint_on: {
                  column: 'InvoiceId',
                  table: ['InvoiceLine'],
                },
              },
            },
          ],
        },
        {
          table: ['InvoiceLine'],
          object_relationships: [
            {
              name: 'Invoice',
              using: {
                foreign_key_constraint_on: 'InvoiceId',
              },
            },
            {
              name: 'Track',
              using: {
                foreign_key_constraint_on: 'TrackId',
              },
            },
          ],
        },
        {
          table: ['MediaType'],
          array_relationships: [
            {
              name: 'Tracks',
              using: {
                foreign_key_constraint_on: {
                  column: 'MediaTypeId',
                  table: ['Track'],
                },
              },
            },
          ],
        },
        {
          table: ['Playlist'],
          array_relationships: [
            {
              name: 'PlaylistTracks',
              using: {
                foreign_key_constraint_on: {
                  column: 'PlaylistId',
                  table: ['PlaylistTrack'],
                },
              },
            },
          ],
        },
        {
          table: ['PlaylistTrack'],
          object_relationships: [
            {
              name: 'Playlist',
              using: {
                foreign_key_constraint_on: 'PlaylistId',
              },
            },
            {
              name: 'Track',
              using: {
                foreign_key_constraint_on: 'TrackId',
              },
            },
          ],
        },
        {
          table: ['Track'],
          object_relationships: [
            {
              name: 'Album',
              using: {
                foreign_key_constraint_on: 'AlbumId',
              },
            },
            {
              name: 'Genre',
              using: {
                foreign_key_constraint_on: 'GenreId',
              },
            },
            {
              name: 'MediaType',
              using: {
                foreign_key_constraint_on: 'MediaTypeId',
              },
            },
          ],
          array_relationships: [
            {
              name: 'InvoiceLines',
              using: {
                foreign_key_constraint_on: {
                  column: 'TrackId',
                  table: ['InvoiceLine'],
                },
              },
            },
            {
              name: 'PlaylistTracks',
              using: {
                foreign_key_constraint_on: {
                  column: 'TrackId',
                  table: ['PlaylistTrack'],
                },
              },
            },
          ],
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
    relationships: [
      {
        name: 'Invoices',
        fromSource: 'MySQ',
        fromTable: ['Customer'],
        relationshipType: 'Array',
        type: 'localRelationship',
        definition: {
          toTable: ['Invoice'],
          toColumns: ['CustomerId'],
          fromTable: ['Customer'],
          fromColumns: ['CustomerId'],
          mapping: {
            CustomerId: 'CustomerId',
          },
        },
      },
      {
        name: 'Employee',
        fromSource: 'MySQ',
        fromTable: ['Customer'],
        relationshipType: 'Object',
        type: 'localRelationship',
        definition: {
          toTable: ['Employee'],
          toColumns: ['EmployeeId'],
          fromTable: ['Customer'],
          fromColumns: ['SupportRepId'],
          mapping: {
            SupportRepId: 'EmployeeId',
          },
        },
      },
    ],
    columns: [
      {
        name: 'CustomerId',
        dataType: 'string',
        consoleDataType: 'string',
        nullable: false,
        isPrimaryKey: true,
        graphQLProperties: {
          name: 'CustomerId',
          scalarType: 'Int',
          graphQLType: 'Int!',
        },
      },
      {
        name: 'FirstName',
        dataType: 'string',
        consoleDataType: 'string',
        nullable: false,
        isPrimaryKey: false,
        graphQLProperties: {
          name: 'FirstName',
          scalarType: 'String',
          graphQLType: 'String!',
        },
      },
      {
        name: 'LastName',
        dataType: 'string',
        consoleDataType: 'string',
        nullable: false,
        isPrimaryKey: false,
        graphQLProperties: {
          name: 'LastName',
          scalarType: 'String',
          graphQLType: 'String!',
        },
      },
      {
        name: 'Company',
        dataType: 'string',
        consoleDataType: 'string',
        nullable: true,
        isPrimaryKey: false,
        graphQLProperties: {
          name: 'Company',
          scalarType: 'String',
          graphQLType: 'String',
        },
      },
      {
        name: 'Address',
        dataType: 'string',
        consoleDataType: 'string',
        nullable: true,
        isPrimaryKey: false,
        graphQLProperties: {
          name: 'Address',
          scalarType: 'String',
          graphQLType: 'String',
        },
      },
      {
        name: 'City',
        dataType: 'string',
        consoleDataType: 'string',
        nullable: true,
        isPrimaryKey: false,
        graphQLProperties: {
          name: 'City',
          scalarType: 'String',
          graphQLType: 'String',
        },
      },
      {
        name: 'State',
        dataType: 'string',
        consoleDataType: 'string',
        nullable: true,
        isPrimaryKey: false,
        graphQLProperties: {
          name: 'State',
          scalarType: 'String',
          graphQLType: 'String',
        },
      },
      {
        name: 'Country',
        dataType: 'string',
        consoleDataType: 'string',
        nullable: true,
        isPrimaryKey: false,
        graphQLProperties: {
          name: 'Country',
          scalarType: 'String',
          graphQLType: 'String',
        },
      },
      {
        name: 'PostalCode',
        dataType: 'string',
        consoleDataType: 'string',
        nullable: true,
        isPrimaryKey: false,
        graphQLProperties: {
          name: 'PostalCode',
          scalarType: 'String',
          graphQLType: 'String',
        },
      },
      {
        name: 'Phone',
        dataType: 'string',
        consoleDataType: 'string',
        nullable: true,
        isPrimaryKey: false,
        graphQLProperties: {
          name: 'Phone',
          scalarType: 'String',
          graphQLType: 'String',
        },
      },
      {
        name: 'Fax',
        dataType: 'string',
        consoleDataType: 'string',
        nullable: true,
        isPrimaryKey: false,
        graphQLProperties: {
          name: 'Fax',
          scalarType: 'String',
          graphQLType: 'String',
        },
      },
      {
        name: 'Email',
        dataType: 'string',
        consoleDataType: 'string',
        nullable: false,
        isPrimaryKey: false,
        graphQLProperties: {
          name: 'Email',
          scalarType: 'String',
          graphQLType: 'String!',
        },
      },
      {
        name: 'SupportRepId',
        dataType: 'string',
        consoleDataType: 'string',
        nullable: true,
        isPrimaryKey: false,
        graphQLProperties: {
          name: 'SupportRepId',
          scalarType: 'Int',
          graphQLType: 'Int',
        },
      },
    ],
  },
  {
    table: ['Employee'],
    dataSource: {
      name: 'MySQ',
      kind: 'mysql8',
      tables: [
        {
          table: ['Album'],
          object_relationships: [
            {
              name: 'Artist',
              using: {
                foreign_key_constraint_on: 'ArtistId',
              },
            },
          ],
          array_relationships: [
            {
              name: 'Tracks',
              using: {
                foreign_key_constraint_on: {
                  column: 'AlbumId',
                  table: ['Track'],
                },
              },
            },
          ],
          select_permissions: [
            {
              role: 'a',
              permission: {
                columns: ['AlbumId', 'Title', 'ArtistId'],
                filter: {},
              },
              comment: '',
            },
            {
              role: 'v',
              permission: {
                columns: [],
                filter: {
                  Artist: {
                    Albums: {
                      Name: {
                        _eq: '',
                      },
                    },
                  },
                },
              },
              comment: '',
            },
          ],
        },
        {
          table: ['Artist'],
          array_relationships: [
            {
              name: 'Albums',
              using: {
                foreign_key_constraint_on: {
                  column: 'ArtistId',
                  table: ['Album'],
                },
              },
            },
          ],
        },
        {
          table: ['Customer'],
          object_relationships: [
            {
              name: 'Employee',
              using: {
                foreign_key_constraint_on: 'SupportRepId',
              },
            },
          ],
          array_relationships: [
            {
              name: 'Invoices',
              using: {
                foreign_key_constraint_on: {
                  column: 'CustomerId',
                  table: ['Invoice'],
                },
              },
            },
          ],
        },
        {
          table: ['Employee'],
          object_relationships: [
            {
              name: 'Employee',
              using: {
                foreign_key_constraint_on: 'ReportsTo',
              },
            },
          ],
          array_relationships: [
            {
              name: 'Customers',
              using: {
                foreign_key_constraint_on: {
                  column: 'SupportRepId',
                  table: ['Customer'],
                },
              },
            },
            {
              name: 'Employees',
              using: {
                foreign_key_constraint_on: {
                  column: 'ReportsTo',
                  table: ['Employee'],
                },
              },
            },
          ],
        },
        {
          table: ['Genre'],
          array_relationships: [
            {
              name: 'Tracks',
              using: {
                foreign_key_constraint_on: {
                  column: 'GenreId',
                  table: ['Track'],
                },
              },
            },
          ],
        },
        {
          table: ['Invoice'],
          object_relationships: [
            {
              name: 'Customer',
              using: {
                foreign_key_constraint_on: 'CustomerId',
              },
            },
          ],
          array_relationships: [
            {
              name: 'InvoiceLines',
              using: {
                foreign_key_constraint_on: {
                  column: 'InvoiceId',
                  table: ['InvoiceLine'],
                },
              },
            },
          ],
        },
        {
          table: ['InvoiceLine'],
          object_relationships: [
            {
              name: 'Invoice',
              using: {
                foreign_key_constraint_on: 'InvoiceId',
              },
            },
            {
              name: 'Track',
              using: {
                foreign_key_constraint_on: 'TrackId',
              },
            },
          ],
        },
        {
          table: ['MediaType'],
          array_relationships: [
            {
              name: 'Tracks',
              using: {
                foreign_key_constraint_on: {
                  column: 'MediaTypeId',
                  table: ['Track'],
                },
              },
            },
          ],
        },
        {
          table: ['Playlist'],
          array_relationships: [
            {
              name: 'PlaylistTracks',
              using: {
                foreign_key_constraint_on: {
                  column: 'PlaylistId',
                  table: ['PlaylistTrack'],
                },
              },
            },
          ],
        },
        {
          table: ['PlaylistTrack'],
          object_relationships: [
            {
              name: 'Playlist',
              using: {
                foreign_key_constraint_on: 'PlaylistId',
              },
            },
            {
              name: 'Track',
              using: {
                foreign_key_constraint_on: 'TrackId',
              },
            },
          ],
        },
        {
          table: ['Track'],
          object_relationships: [
            {
              name: 'Album',
              using: {
                foreign_key_constraint_on: 'AlbumId',
              },
            },
            {
              name: 'Genre',
              using: {
                foreign_key_constraint_on: 'GenreId',
              },
            },
            {
              name: 'MediaType',
              using: {
                foreign_key_constraint_on: 'MediaTypeId',
              },
            },
          ],
          array_relationships: [
            {
              name: 'InvoiceLines',
              using: {
                foreign_key_constraint_on: {
                  column: 'TrackId',
                  table: ['InvoiceLine'],
                },
              },
            },
            {
              name: 'PlaylistTracks',
              using: {
                foreign_key_constraint_on: {
                  column: 'TrackId',
                  table: ['PlaylistTrack'],
                },
              },
            },
          ],
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
    relationships: [
      {
        name: 'Customers',
        fromSource: 'MySQ',
        fromTable: ['Employee'],
        relationshipType: 'Array',
        type: 'localRelationship',
        definition: {
          toTable: ['Customer'],
          toColumns: ['SupportRepId'],
          fromTable: ['Employee'],
          fromColumns: ['EmployeeId'],
          mapping: {
            SupportRepId: 'EmployeeId',
          },
        },
      },
      {
        name: 'Employees',
        fromSource: 'MySQ',
        fromTable: ['Employee'],
        relationshipType: 'Array',
        type: 'localRelationship',
        definition: {
          toTable: ['Employee'],
          toColumns: ['ReportsTo'],
          fromTable: ['Employee'],
          fromColumns: ['EmployeeId'],
          mapping: {
            ReportsTo: 'EmployeeId',
          },
        },
      },
      {
        name: 'Employee',
        fromSource: 'MySQ',
        fromTable: ['Employee'],
        relationshipType: 'Object',
        type: 'localRelationship',
        definition: {
          toTable: ['Employee'],
          toColumns: ['EmployeeId'],
          fromTable: ['Employee'],
          fromColumns: ['ReportsTo'],
          mapping: {
            ReportsTo: 'EmployeeId',
          },
        },
      },
    ],
    columns: [
      {
        name: 'EmployeeId',
        dataType: 'string',
        consoleDataType: 'string',
        nullable: false,
        isPrimaryKey: true,
        graphQLProperties: {
          name: 'EmployeeId',
          scalarType: 'Int',
          graphQLType: 'Int!',
        },
      },
      {
        name: 'LastName',
        dataType: 'string',
        consoleDataType: 'string',
        nullable: false,
        isPrimaryKey: false,
        graphQLProperties: {
          name: 'LastName',
          scalarType: 'String',
          graphQLType: 'String!',
        },
      },
      {
        name: 'FirstName',
        dataType: 'string',
        consoleDataType: 'string',
        nullable: false,
        isPrimaryKey: false,
        graphQLProperties: {
          name: 'FirstName',
          scalarType: 'String',
          graphQLType: 'String!',
        },
      },
      {
        name: 'Title',
        dataType: 'string',
        consoleDataType: 'string',
        nullable: true,
        isPrimaryKey: false,
        graphQLProperties: {
          name: 'Title',
          scalarType: 'String',
          graphQLType: 'String',
        },
      },
      {
        name: 'ReportsTo',
        dataType: 'string',
        consoleDataType: 'string',
        nullable: true,
        isPrimaryKey: false,
        graphQLProperties: {
          name: 'ReportsTo',
          scalarType: 'Int',
          graphQLType: 'Int',
        },
      },
      {
        name: 'BirthDate',
        dataType: 'datetime',
        consoleDataType: 'string',
        nullable: true,
        isPrimaryKey: false,
        graphQLProperties: {
          name: 'BirthDate',
          scalarType: 'datetime',
          graphQLType: 'datetime',
        },
      },
      {
        name: 'HireDate',
        dataType: 'datetime',
        consoleDataType: 'string',
        nullable: true,
        isPrimaryKey: false,
        graphQLProperties: {
          name: 'HireDate',
          scalarType: 'datetime',
          graphQLType: 'datetime',
        },
      },
      {
        name: 'Address',
        dataType: 'string',
        consoleDataType: 'string',
        nullable: true,
        isPrimaryKey: false,
        graphQLProperties: {
          name: 'Address',
          scalarType: 'String',
          graphQLType: 'String',
        },
      },
      {
        name: 'City',
        dataType: 'string',
        consoleDataType: 'string',
        nullable: true,
        isPrimaryKey: false,
        graphQLProperties: {
          name: 'City',
          scalarType: 'String',
          graphQLType: 'String',
        },
      },
      {
        name: 'State',
        dataType: 'string',
        consoleDataType: 'string',
        nullable: true,
        isPrimaryKey: false,
        graphQLProperties: {
          name: 'State',
          scalarType: 'String',
          graphQLType: 'String',
        },
      },
      {
        name: 'Country',
        dataType: 'string',
        consoleDataType: 'string',
        nullable: true,
        isPrimaryKey: false,
        graphQLProperties: {
          name: 'Country',
          scalarType: 'String',
          graphQLType: 'String',
        },
      },
      {
        name: 'PostalCode',
        dataType: 'string',
        consoleDataType: 'string',
        nullable: true,
        isPrimaryKey: false,
        graphQLProperties: {
          name: 'PostalCode',
          scalarType: 'String',
          graphQLType: 'String',
        },
      },
      {
        name: 'Phone',
        dataType: 'string',
        consoleDataType: 'string',
        nullable: true,
        isPrimaryKey: false,
        graphQLProperties: {
          name: 'Phone',
          scalarType: 'String',
          graphQLType: 'String',
        },
      },
      {
        name: 'Fax',
        dataType: 'string',
        consoleDataType: 'string',
        nullable: true,
        isPrimaryKey: false,
        graphQLProperties: {
          name: 'Fax',
          scalarType: 'String',
          graphQLType: 'String',
        },
      },
      {
        name: 'Email',
        dataType: 'string',
        consoleDataType: 'string',
        nullable: true,
        isPrimaryKey: false,
        graphQLProperties: {
          name: 'Email',
          scalarType: 'String',
          graphQLType: 'String',
        },
      },
    ],
  },
  {
    table: ['Genre'],
    dataSource: {
      name: 'MySQ',
      kind: 'mysql8',
      tables: [
        {
          table: ['Album'],
          object_relationships: [
            {
              name: 'Artist',
              using: {
                foreign_key_constraint_on: 'ArtistId',
              },
            },
          ],
          array_relationships: [
            {
              name: 'Tracks',
              using: {
                foreign_key_constraint_on: {
                  column: 'AlbumId',
                  table: ['Track'],
                },
              },
            },
          ],
          select_permissions: [
            {
              role: 'a',
              permission: {
                columns: ['AlbumId', 'Title', 'ArtistId'],
                filter: {},
              },
              comment: '',
            },
            {
              role: 'v',
              permission: {
                columns: [],
                filter: {
                  Artist: {
                    Albums: {
                      Name: {
                        _eq: '',
                      },
                    },
                  },
                },
              },
              comment: '',
            },
          ],
        },
        {
          table: ['Artist'],
          array_relationships: [
            {
              name: 'Albums',
              using: {
                foreign_key_constraint_on: {
                  column: 'ArtistId',
                  table: ['Album'],
                },
              },
            },
          ],
        },
        {
          table: ['Customer'],
          object_relationships: [
            {
              name: 'Employee',
              using: {
                foreign_key_constraint_on: 'SupportRepId',
              },
            },
          ],
          array_relationships: [
            {
              name: 'Invoices',
              using: {
                foreign_key_constraint_on: {
                  column: 'CustomerId',
                  table: ['Invoice'],
                },
              },
            },
          ],
        },
        {
          table: ['Employee'],
          object_relationships: [
            {
              name: 'Employee',
              using: {
                foreign_key_constraint_on: 'ReportsTo',
              },
            },
          ],
          array_relationships: [
            {
              name: 'Customers',
              using: {
                foreign_key_constraint_on: {
                  column: 'SupportRepId',
                  table: ['Customer'],
                },
              },
            },
            {
              name: 'Employees',
              using: {
                foreign_key_constraint_on: {
                  column: 'ReportsTo',
                  table: ['Employee'],
                },
              },
            },
          ],
        },
        {
          table: ['Genre'],
          array_relationships: [
            {
              name: 'Tracks',
              using: {
                foreign_key_constraint_on: {
                  column: 'GenreId',
                  table: ['Track'],
                },
              },
            },
          ],
        },
        {
          table: ['Invoice'],
          object_relationships: [
            {
              name: 'Customer',
              using: {
                foreign_key_constraint_on: 'CustomerId',
              },
            },
          ],
          array_relationships: [
            {
              name: 'InvoiceLines',
              using: {
                foreign_key_constraint_on: {
                  column: 'InvoiceId',
                  table: ['InvoiceLine'],
                },
              },
            },
          ],
        },
        {
          table: ['InvoiceLine'],
          object_relationships: [
            {
              name: 'Invoice',
              using: {
                foreign_key_constraint_on: 'InvoiceId',
              },
            },
            {
              name: 'Track',
              using: {
                foreign_key_constraint_on: 'TrackId',
              },
            },
          ],
        },
        {
          table: ['MediaType'],
          array_relationships: [
            {
              name: 'Tracks',
              using: {
                foreign_key_constraint_on: {
                  column: 'MediaTypeId',
                  table: ['Track'],
                },
              },
            },
          ],
        },
        {
          table: ['Playlist'],
          array_relationships: [
            {
              name: 'PlaylistTracks',
              using: {
                foreign_key_constraint_on: {
                  column: 'PlaylistId',
                  table: ['PlaylistTrack'],
                },
              },
            },
          ],
        },
        {
          table: ['PlaylistTrack'],
          object_relationships: [
            {
              name: 'Playlist',
              using: {
                foreign_key_constraint_on: 'PlaylistId',
              },
            },
            {
              name: 'Track',
              using: {
                foreign_key_constraint_on: 'TrackId',
              },
            },
          ],
        },
        {
          table: ['Track'],
          object_relationships: [
            {
              name: 'Album',
              using: {
                foreign_key_constraint_on: 'AlbumId',
              },
            },
            {
              name: 'Genre',
              using: {
                foreign_key_constraint_on: 'GenreId',
              },
            },
            {
              name: 'MediaType',
              using: {
                foreign_key_constraint_on: 'MediaTypeId',
              },
            },
          ],
          array_relationships: [
            {
              name: 'InvoiceLines',
              using: {
                foreign_key_constraint_on: {
                  column: 'TrackId',
                  table: ['InvoiceLine'],
                },
              },
            },
            {
              name: 'PlaylistTracks',
              using: {
                foreign_key_constraint_on: {
                  column: 'TrackId',
                  table: ['PlaylistTrack'],
                },
              },
            },
          ],
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
    relationships: [
      {
        name: 'Tracks',
        fromSource: 'MySQ',
        fromTable: ['Genre'],
        relationshipType: 'Array',
        type: 'localRelationship',
        definition: {
          toTable: ['Track'],
          toColumns: ['GenreId'],
          fromTable: ['Genre'],
          fromColumns: ['GenreId'],
          mapping: {
            GenreId: 'GenreId',
          },
        },
      },
    ],
    columns: [
      {
        name: 'GenreId',
        dataType: 'string',
        consoleDataType: 'string',
        nullable: false,
        isPrimaryKey: true,
        graphQLProperties: {
          name: 'GenreId',
          scalarType: 'Int',
          graphQLType: 'Int!',
        },
      },
      {
        name: 'Name',
        dataType: 'string',
        consoleDataType: 'string',
        nullable: true,
        isPrimaryKey: false,
        graphQLProperties: {
          name: 'Name',
          scalarType: 'String',
          graphQLType: 'String',
        },
      },
    ],
  },
  {
    table: ['Invoice'],
    dataSource: {
      name: 'MySQ',
      kind: 'mysql8',
      tables: [
        {
          table: ['Album'],
          object_relationships: [
            {
              name: 'Artist',
              using: {
                foreign_key_constraint_on: 'ArtistId',
              },
            },
          ],
          array_relationships: [
            {
              name: 'Tracks',
              using: {
                foreign_key_constraint_on: {
                  column: 'AlbumId',
                  table: ['Track'],
                },
              },
            },
          ],
          select_permissions: [
            {
              role: 'a',
              permission: {
                columns: ['AlbumId', 'Title', 'ArtistId'],
                filter: {},
              },
              comment: '',
            },
            {
              role: 'v',
              permission: {
                columns: [],
                filter: {
                  Artist: {
                    Albums: {
                      Name: {
                        _eq: '',
                      },
                    },
                  },
                },
              },
              comment: '',
            },
          ],
        },
        {
          table: ['Artist'],
          array_relationships: [
            {
              name: 'Albums',
              using: {
                foreign_key_constraint_on: {
                  column: 'ArtistId',
                  table: ['Album'],
                },
              },
            },
          ],
        },
        {
          table: ['Customer'],
          object_relationships: [
            {
              name: 'Employee',
              using: {
                foreign_key_constraint_on: 'SupportRepId',
              },
            },
          ],
          array_relationships: [
            {
              name: 'Invoices',
              using: {
                foreign_key_constraint_on: {
                  column: 'CustomerId',
                  table: ['Invoice'],
                },
              },
            },
          ],
        },
        {
          table: ['Employee'],
          object_relationships: [
            {
              name: 'Employee',
              using: {
                foreign_key_constraint_on: 'ReportsTo',
              },
            },
          ],
          array_relationships: [
            {
              name: 'Customers',
              using: {
                foreign_key_constraint_on: {
                  column: 'SupportRepId',
                  table: ['Customer'],
                },
              },
            },
            {
              name: 'Employees',
              using: {
                foreign_key_constraint_on: {
                  column: 'ReportsTo',
                  table: ['Employee'],
                },
              },
            },
          ],
        },
        {
          table: ['Genre'],
          array_relationships: [
            {
              name: 'Tracks',
              using: {
                foreign_key_constraint_on: {
                  column: 'GenreId',
                  table: ['Track'],
                },
              },
            },
          ],
        },
        {
          table: ['Invoice'],
          object_relationships: [
            {
              name: 'Customer',
              using: {
                foreign_key_constraint_on: 'CustomerId',
              },
            },
          ],
          array_relationships: [
            {
              name: 'InvoiceLines',
              using: {
                foreign_key_constraint_on: {
                  column: 'InvoiceId',
                  table: ['InvoiceLine'],
                },
              },
            },
          ],
        },
        {
          table: ['InvoiceLine'],
          object_relationships: [
            {
              name: 'Invoice',
              using: {
                foreign_key_constraint_on: 'InvoiceId',
              },
            },
            {
              name: 'Track',
              using: {
                foreign_key_constraint_on: 'TrackId',
              },
            },
          ],
        },
        {
          table: ['MediaType'],
          array_relationships: [
            {
              name: 'Tracks',
              using: {
                foreign_key_constraint_on: {
                  column: 'MediaTypeId',
                  table: ['Track'],
                },
              },
            },
          ],
        },
        {
          table: ['Playlist'],
          array_relationships: [
            {
              name: 'PlaylistTracks',
              using: {
                foreign_key_constraint_on: {
                  column: 'PlaylistId',
                  table: ['PlaylistTrack'],
                },
              },
            },
          ],
        },
        {
          table: ['PlaylistTrack'],
          object_relationships: [
            {
              name: 'Playlist',
              using: {
                foreign_key_constraint_on: 'PlaylistId',
              },
            },
            {
              name: 'Track',
              using: {
                foreign_key_constraint_on: 'TrackId',
              },
            },
          ],
        },
        {
          table: ['Track'],
          object_relationships: [
            {
              name: 'Album',
              using: {
                foreign_key_constraint_on: 'AlbumId',
              },
            },
            {
              name: 'Genre',
              using: {
                foreign_key_constraint_on: 'GenreId',
              },
            },
            {
              name: 'MediaType',
              using: {
                foreign_key_constraint_on: 'MediaTypeId',
              },
            },
          ],
          array_relationships: [
            {
              name: 'InvoiceLines',
              using: {
                foreign_key_constraint_on: {
                  column: 'TrackId',
                  table: ['InvoiceLine'],
                },
              },
            },
            {
              name: 'PlaylistTracks',
              using: {
                foreign_key_constraint_on: {
                  column: 'TrackId',
                  table: ['PlaylistTrack'],
                },
              },
            },
          ],
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
    relationships: [
      {
        name: 'InvoiceLines',
        fromSource: 'MySQ',
        fromTable: ['Invoice'],
        relationshipType: 'Array',
        type: 'localRelationship',
        definition: {
          toTable: ['InvoiceLine'],
          toColumns: ['InvoiceId'],
          fromTable: ['Invoice'],
          fromColumns: ['InvoiceId'],
          mapping: {
            InvoiceId: 'InvoiceId',
          },
        },
      },
      {
        name: 'Customer',
        fromSource: 'MySQ',
        fromTable: ['Invoice'],
        relationshipType: 'Object',
        type: 'localRelationship',
        definition: {
          toTable: ['Customer'],
          toColumns: ['CustomerId'],
          fromTable: ['Invoice'],
          fromColumns: ['CustomerId'],
          mapping: {
            CustomerId: 'CustomerId',
          },
        },
      },
    ],
    columns: [
      {
        name: 'InvoiceId',
        dataType: 'string',
        consoleDataType: 'string',
        nullable: false,
        isPrimaryKey: true,
        graphQLProperties: {
          name: 'InvoiceId',
          scalarType: 'Int',
          graphQLType: 'Int!',
        },
      },
      {
        name: 'CustomerId',
        dataType: 'string',
        consoleDataType: 'string',
        nullable: false,
        isPrimaryKey: false,
        graphQLProperties: {
          name: 'CustomerId',
          scalarType: 'Int',
          graphQLType: 'Int!',
        },
      },
      {
        name: 'InvoiceDate',
        dataType: 'datetime',
        consoleDataType: 'string',
        nullable: false,
        isPrimaryKey: false,
        graphQLProperties: {
          name: 'InvoiceDate',
          scalarType: 'datetime',
          graphQLType: 'datetime!',
        },
      },
      {
        name: 'BillingAddress',
        dataType: 'string',
        consoleDataType: 'string',
        nullable: true,
        isPrimaryKey: false,
        graphQLProperties: {
          name: 'BillingAddress',
          scalarType: 'String',
          graphQLType: 'String',
        },
      },
      {
        name: 'BillingCity',
        dataType: 'string',
        consoleDataType: 'string',
        nullable: true,
        isPrimaryKey: false,
        graphQLProperties: {
          name: 'BillingCity',
          scalarType: 'String',
          graphQLType: 'String',
        },
      },
      {
        name: 'BillingState',
        dataType: 'string',
        consoleDataType: 'string',
        nullable: true,
        isPrimaryKey: false,
        graphQLProperties: {
          name: 'BillingState',
          scalarType: 'String',
          graphQLType: 'String',
        },
      },
      {
        name: 'BillingCountry',
        dataType: 'string',
        consoleDataType: 'string',
        nullable: true,
        isPrimaryKey: false,
        graphQLProperties: {
          name: 'BillingCountry',
          scalarType: 'String',
          graphQLType: 'String',
        },
      },
      {
        name: 'BillingPostalCode',
        dataType: 'string',
        consoleDataType: 'string',
        nullable: true,
        isPrimaryKey: false,
        graphQLProperties: {
          name: 'BillingPostalCode',
          scalarType: 'String',
          graphQLType: 'String',
        },
      },
      {
        name: 'Total',
        dataType: 'number',
        consoleDataType: 'string',
        nullable: false,
        isPrimaryKey: false,
        graphQLProperties: {
          name: 'Total',
          scalarType: 'Float',
          graphQLType: 'Float!',
        },
      },
    ],
  },
  {
    table: ['InvoiceLine'],
    dataSource: {
      name: 'MySQ',
      kind: 'mysql8',
      tables: [
        {
          table: ['Album'],
          object_relationships: [
            {
              name: 'Artist',
              using: {
                foreign_key_constraint_on: 'ArtistId',
              },
            },
          ],
          array_relationships: [
            {
              name: 'Tracks',
              using: {
                foreign_key_constraint_on: {
                  column: 'AlbumId',
                  table: ['Track'],
                },
              },
            },
          ],
          select_permissions: [
            {
              role: 'a',
              permission: {
                columns: ['AlbumId', 'Title', 'ArtistId'],
                filter: {},
              },
              comment: '',
            },
            {
              role: 'v',
              permission: {
                columns: [],
                filter: {
                  Artist: {
                    Albums: {
                      Name: {
                        _eq: '',
                      },
                    },
                  },
                },
              },
              comment: '',
            },
          ],
        },
        {
          table: ['Artist'],
          array_relationships: [
            {
              name: 'Albums',
              using: {
                foreign_key_constraint_on: {
                  column: 'ArtistId',
                  table: ['Album'],
                },
              },
            },
          ],
        },
        {
          table: ['Customer'],
          object_relationships: [
            {
              name: 'Employee',
              using: {
                foreign_key_constraint_on: 'SupportRepId',
              },
            },
          ],
          array_relationships: [
            {
              name: 'Invoices',
              using: {
                foreign_key_constraint_on: {
                  column: 'CustomerId',
                  table: ['Invoice'],
                },
              },
            },
          ],
        },
        {
          table: ['Employee'],
          object_relationships: [
            {
              name: 'Employee',
              using: {
                foreign_key_constraint_on: 'ReportsTo',
              },
            },
          ],
          array_relationships: [
            {
              name: 'Customers',
              using: {
                foreign_key_constraint_on: {
                  column: 'SupportRepId',
                  table: ['Customer'],
                },
              },
            },
            {
              name: 'Employees',
              using: {
                foreign_key_constraint_on: {
                  column: 'ReportsTo',
                  table: ['Employee'],
                },
              },
            },
          ],
        },
        {
          table: ['Genre'],
          array_relationships: [
            {
              name: 'Tracks',
              using: {
                foreign_key_constraint_on: {
                  column: 'GenreId',
                  table: ['Track'],
                },
              },
            },
          ],
        },
        {
          table: ['Invoice'],
          object_relationships: [
            {
              name: 'Customer',
              using: {
                foreign_key_constraint_on: 'CustomerId',
              },
            },
          ],
          array_relationships: [
            {
              name: 'InvoiceLines',
              using: {
                foreign_key_constraint_on: {
                  column: 'InvoiceId',
                  table: ['InvoiceLine'],
                },
              },
            },
          ],
        },
        {
          table: ['InvoiceLine'],
          object_relationships: [
            {
              name: 'Invoice',
              using: {
                foreign_key_constraint_on: 'InvoiceId',
              },
            },
            {
              name: 'Track',
              using: {
                foreign_key_constraint_on: 'TrackId',
              },
            },
          ],
        },
        {
          table: ['MediaType'],
          array_relationships: [
            {
              name: 'Tracks',
              using: {
                foreign_key_constraint_on: {
                  column: 'MediaTypeId',
                  table: ['Track'],
                },
              },
            },
          ],
        },
        {
          table: ['Playlist'],
          array_relationships: [
            {
              name: 'PlaylistTracks',
              using: {
                foreign_key_constraint_on: {
                  column: 'PlaylistId',
                  table: ['PlaylistTrack'],
                },
              },
            },
          ],
        },
        {
          table: ['PlaylistTrack'],
          object_relationships: [
            {
              name: 'Playlist',
              using: {
                foreign_key_constraint_on: 'PlaylistId',
              },
            },
            {
              name: 'Track',
              using: {
                foreign_key_constraint_on: 'TrackId',
              },
            },
          ],
        },
        {
          table: ['Track'],
          object_relationships: [
            {
              name: 'Album',
              using: {
                foreign_key_constraint_on: 'AlbumId',
              },
            },
            {
              name: 'Genre',
              using: {
                foreign_key_constraint_on: 'GenreId',
              },
            },
            {
              name: 'MediaType',
              using: {
                foreign_key_constraint_on: 'MediaTypeId',
              },
            },
          ],
          array_relationships: [
            {
              name: 'InvoiceLines',
              using: {
                foreign_key_constraint_on: {
                  column: 'TrackId',
                  table: ['InvoiceLine'],
                },
              },
            },
            {
              name: 'PlaylistTracks',
              using: {
                foreign_key_constraint_on: {
                  column: 'TrackId',
                  table: ['PlaylistTrack'],
                },
              },
            },
          ],
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
    relationships: [
      {
        name: 'Invoice',
        fromSource: 'MySQ',
        fromTable: ['InvoiceLine'],
        relationshipType: 'Object',
        type: 'localRelationship',
        definition: {
          toTable: ['Invoice'],
          toColumns: ['InvoiceId'],
          fromTable: ['InvoiceLine'],
          fromColumns: ['InvoiceId'],
          mapping: {
            InvoiceId: 'InvoiceId',
          },
        },
      },
      {
        name: 'Track',
        fromSource: 'MySQ',
        fromTable: ['InvoiceLine'],
        relationshipType: 'Object',
        type: 'localRelationship',
        definition: {
          toTable: ['Track'],
          toColumns: ['TrackId'],
          fromTable: ['InvoiceLine'],
          fromColumns: ['TrackId'],
          mapping: {
            TrackId: 'TrackId',
          },
        },
      },
    ],
    columns: [
      {
        name: 'InvoiceLineId',
        dataType: 'string',
        consoleDataType: 'string',
        nullable: false,
        isPrimaryKey: true,
        graphQLProperties: {
          name: 'InvoiceLineId',
          scalarType: 'Int',
          graphQLType: 'Int!',
        },
      },
      {
        name: 'InvoiceId',
        dataType: 'string',
        consoleDataType: 'string',
        nullable: false,
        isPrimaryKey: false,
        graphQLProperties: {
          name: 'InvoiceId',
          scalarType: 'Int',
          graphQLType: 'Int!',
        },
      },
      {
        name: 'TrackId',
        dataType: 'string',
        consoleDataType: 'string',
        nullable: false,
        isPrimaryKey: false,
        graphQLProperties: {
          name: 'TrackId',
          scalarType: 'Int',
          graphQLType: 'Int!',
        },
      },
      {
        name: 'UnitPrice',
        dataType: 'number',
        consoleDataType: 'string',
        nullable: false,
        isPrimaryKey: false,
        graphQLProperties: {
          name: 'UnitPrice',
          scalarType: 'Float',
          graphQLType: 'Float!',
        },
      },
      {
        name: 'Quantity',
        dataType: 'string',
        consoleDataType: 'string',
        nullable: false,
        isPrimaryKey: false,
        graphQLProperties: {
          name: 'Quantity',
          scalarType: 'Int',
          graphQLType: 'Int!',
        },
      },
    ],
  },
  {
    table: ['MediaType'],
    dataSource: {
      name: 'MySQ',
      kind: 'mysql8',
      tables: [
        {
          table: ['Album'],
          object_relationships: [
            {
              name: 'Artist',
              using: {
                foreign_key_constraint_on: 'ArtistId',
              },
            },
          ],
          array_relationships: [
            {
              name: 'Tracks',
              using: {
                foreign_key_constraint_on: {
                  column: 'AlbumId',
                  table: ['Track'],
                },
              },
            },
          ],
          select_permissions: [
            {
              role: 'a',
              permission: {
                columns: ['AlbumId', 'Title', 'ArtistId'],
                filter: {},
              },
              comment: '',
            },
            {
              role: 'v',
              permission: {
                columns: [],
                filter: {
                  Artist: {
                    Albums: {
                      Name: {
                        _eq: '',
                      },
                    },
                  },
                },
              },
              comment: '',
            },
          ],
        },
        {
          table: ['Artist'],
          array_relationships: [
            {
              name: 'Albums',
              using: {
                foreign_key_constraint_on: {
                  column: 'ArtistId',
                  table: ['Album'],
                },
              },
            },
          ],
        },
        {
          table: ['Customer'],
          object_relationships: [
            {
              name: 'Employee',
              using: {
                foreign_key_constraint_on: 'SupportRepId',
              },
            },
          ],
          array_relationships: [
            {
              name: 'Invoices',
              using: {
                foreign_key_constraint_on: {
                  column: 'CustomerId',
                  table: ['Invoice'],
                },
              },
            },
          ],
        },
        {
          table: ['Employee'],
          object_relationships: [
            {
              name: 'Employee',
              using: {
                foreign_key_constraint_on: 'ReportsTo',
              },
            },
          ],
          array_relationships: [
            {
              name: 'Customers',
              using: {
                foreign_key_constraint_on: {
                  column: 'SupportRepId',
                  table: ['Customer'],
                },
              },
            },
            {
              name: 'Employees',
              using: {
                foreign_key_constraint_on: {
                  column: 'ReportsTo',
                  table: ['Employee'],
                },
              },
            },
          ],
        },
        {
          table: ['Genre'],
          array_relationships: [
            {
              name: 'Tracks',
              using: {
                foreign_key_constraint_on: {
                  column: 'GenreId',
                  table: ['Track'],
                },
              },
            },
          ],
        },
        {
          table: ['Invoice'],
          object_relationships: [
            {
              name: 'Customer',
              using: {
                foreign_key_constraint_on: 'CustomerId',
              },
            },
          ],
          array_relationships: [
            {
              name: 'InvoiceLines',
              using: {
                foreign_key_constraint_on: {
                  column: 'InvoiceId',
                  table: ['InvoiceLine'],
                },
              },
            },
          ],
        },
        {
          table: ['InvoiceLine'],
          object_relationships: [
            {
              name: 'Invoice',
              using: {
                foreign_key_constraint_on: 'InvoiceId',
              },
            },
            {
              name: 'Track',
              using: {
                foreign_key_constraint_on: 'TrackId',
              },
            },
          ],
        },
        {
          table: ['MediaType'],
          array_relationships: [
            {
              name: 'Tracks',
              using: {
                foreign_key_constraint_on: {
                  column: 'MediaTypeId',
                  table: ['Track'],
                },
              },
            },
          ],
        },
        {
          table: ['Playlist'],
          array_relationships: [
            {
              name: 'PlaylistTracks',
              using: {
                foreign_key_constraint_on: {
                  column: 'PlaylistId',
                  table: ['PlaylistTrack'],
                },
              },
            },
          ],
        },
        {
          table: ['PlaylistTrack'],
          object_relationships: [
            {
              name: 'Playlist',
              using: {
                foreign_key_constraint_on: 'PlaylistId',
              },
            },
            {
              name: 'Track',
              using: {
                foreign_key_constraint_on: 'TrackId',
              },
            },
          ],
        },
        {
          table: ['Track'],
          object_relationships: [
            {
              name: 'Album',
              using: {
                foreign_key_constraint_on: 'AlbumId',
              },
            },
            {
              name: 'Genre',
              using: {
                foreign_key_constraint_on: 'GenreId',
              },
            },
            {
              name: 'MediaType',
              using: {
                foreign_key_constraint_on: 'MediaTypeId',
              },
            },
          ],
          array_relationships: [
            {
              name: 'InvoiceLines',
              using: {
                foreign_key_constraint_on: {
                  column: 'TrackId',
                  table: ['InvoiceLine'],
                },
              },
            },
            {
              name: 'PlaylistTracks',
              using: {
                foreign_key_constraint_on: {
                  column: 'TrackId',
                  table: ['PlaylistTrack'],
                },
              },
            },
          ],
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
    relationships: [
      {
        name: 'Tracks',
        fromSource: 'MySQ',
        fromTable: ['MediaType'],
        relationshipType: 'Array',
        type: 'localRelationship',
        definition: {
          toTable: ['Track'],
          toColumns: ['MediaTypeId'],
          fromTable: ['MediaType'],
          fromColumns: ['MediaTypeId'],
          mapping: {
            MediaTypeId: 'MediaTypeId',
          },
        },
      },
    ],
    columns: [
      {
        name: 'MediaTypeId',
        dataType: 'string',
        consoleDataType: 'string',
        nullable: false,
        isPrimaryKey: true,
        graphQLProperties: {
          name: 'MediaTypeId',
          scalarType: 'Int',
          graphQLType: 'Int!',
        },
      },
      {
        name: 'Name',
        dataType: 'string',
        consoleDataType: 'string',
        nullable: true,
        isPrimaryKey: false,
        graphQLProperties: {
          name: 'Name',
          scalarType: 'String',
          graphQLType: 'String',
        },
      },
    ],
  },
  {
    table: ['Playlist'],
    dataSource: {
      name: 'MySQ',
      kind: 'mysql8',
      tables: [
        {
          table: ['Album'],
          object_relationships: [
            {
              name: 'Artist',
              using: {
                foreign_key_constraint_on: 'ArtistId',
              },
            },
          ],
          array_relationships: [
            {
              name: 'Tracks',
              using: {
                foreign_key_constraint_on: {
                  column: 'AlbumId',
                  table: ['Track'],
                },
              },
            },
          ],
          select_permissions: [
            {
              role: 'a',
              permission: {
                columns: ['AlbumId', 'Title', 'ArtistId'],
                filter: {},
              },
              comment: '',
            },
            {
              role: 'v',
              permission: {
                columns: [],
                filter: {
                  Artist: {
                    Albums: {
                      Name: {
                        _eq: '',
                      },
                    },
                  },
                },
              },
              comment: '',
            },
          ],
        },
        {
          table: ['Artist'],
          array_relationships: [
            {
              name: 'Albums',
              using: {
                foreign_key_constraint_on: {
                  column: 'ArtistId',
                  table: ['Album'],
                },
              },
            },
          ],
        },
        {
          table: ['Customer'],
          object_relationships: [
            {
              name: 'Employee',
              using: {
                foreign_key_constraint_on: 'SupportRepId',
              },
            },
          ],
          array_relationships: [
            {
              name: 'Invoices',
              using: {
                foreign_key_constraint_on: {
                  column: 'CustomerId',
                  table: ['Invoice'],
                },
              },
            },
          ],
        },
        {
          table: ['Employee'],
          object_relationships: [
            {
              name: 'Employee',
              using: {
                foreign_key_constraint_on: 'ReportsTo',
              },
            },
          ],
          array_relationships: [
            {
              name: 'Customers',
              using: {
                foreign_key_constraint_on: {
                  column: 'SupportRepId',
                  table: ['Customer'],
                },
              },
            },
            {
              name: 'Employees',
              using: {
                foreign_key_constraint_on: {
                  column: 'ReportsTo',
                  table: ['Employee'],
                },
              },
            },
          ],
        },
        {
          table: ['Genre'],
          array_relationships: [
            {
              name: 'Tracks',
              using: {
                foreign_key_constraint_on: {
                  column: 'GenreId',
                  table: ['Track'],
                },
              },
            },
          ],
        },
        {
          table: ['Invoice'],
          object_relationships: [
            {
              name: 'Customer',
              using: {
                foreign_key_constraint_on: 'CustomerId',
              },
            },
          ],
          array_relationships: [
            {
              name: 'InvoiceLines',
              using: {
                foreign_key_constraint_on: {
                  column: 'InvoiceId',
                  table: ['InvoiceLine'],
                },
              },
            },
          ],
        },
        {
          table: ['InvoiceLine'],
          object_relationships: [
            {
              name: 'Invoice',
              using: {
                foreign_key_constraint_on: 'InvoiceId',
              },
            },
            {
              name: 'Track',
              using: {
                foreign_key_constraint_on: 'TrackId',
              },
            },
          ],
        },
        {
          table: ['MediaType'],
          array_relationships: [
            {
              name: 'Tracks',
              using: {
                foreign_key_constraint_on: {
                  column: 'MediaTypeId',
                  table: ['Track'],
                },
              },
            },
          ],
        },
        {
          table: ['Playlist'],
          array_relationships: [
            {
              name: 'PlaylistTracks',
              using: {
                foreign_key_constraint_on: {
                  column: 'PlaylistId',
                  table: ['PlaylistTrack'],
                },
              },
            },
          ],
        },
        {
          table: ['PlaylistTrack'],
          object_relationships: [
            {
              name: 'Playlist',
              using: {
                foreign_key_constraint_on: 'PlaylistId',
              },
            },
            {
              name: 'Track',
              using: {
                foreign_key_constraint_on: 'TrackId',
              },
            },
          ],
        },
        {
          table: ['Track'],
          object_relationships: [
            {
              name: 'Album',
              using: {
                foreign_key_constraint_on: 'AlbumId',
              },
            },
            {
              name: 'Genre',
              using: {
                foreign_key_constraint_on: 'GenreId',
              },
            },
            {
              name: 'MediaType',
              using: {
                foreign_key_constraint_on: 'MediaTypeId',
              },
            },
          ],
          array_relationships: [
            {
              name: 'InvoiceLines',
              using: {
                foreign_key_constraint_on: {
                  column: 'TrackId',
                  table: ['InvoiceLine'],
                },
              },
            },
            {
              name: 'PlaylistTracks',
              using: {
                foreign_key_constraint_on: {
                  column: 'TrackId',
                  table: ['PlaylistTrack'],
                },
              },
            },
          ],
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
    relationships: [
      {
        name: 'PlaylistTracks',
        fromSource: 'MySQ',
        fromTable: ['Playlist'],
        relationshipType: 'Array',
        type: 'localRelationship',
        definition: {
          toTable: ['PlaylistTrack'],
          toColumns: ['PlaylistId'],
          fromTable: ['Playlist'],
          fromColumns: ['PlaylistId'],
          mapping: {
            PlaylistId: 'PlaylistId',
          },
        },
      },
    ],
    columns: [
      {
        name: 'PlaylistId',
        dataType: 'string',
        consoleDataType: 'string',
        nullable: false,
        isPrimaryKey: true,
        graphQLProperties: {
          name: 'PlaylistId',
          scalarType: 'Int',
          graphQLType: 'Int!',
        },
      },
      {
        name: 'Name',
        dataType: 'string',
        consoleDataType: 'string',
        nullable: true,
        isPrimaryKey: false,
        graphQLProperties: {
          name: 'Name',
          scalarType: 'String',
          graphQLType: 'String',
        },
      },
    ],
  },
  {
    table: ['PlaylistTrack'],
    dataSource: {
      name: 'MySQ',
      kind: 'mysql8',
      tables: [
        {
          table: ['Album'],
          object_relationships: [
            {
              name: 'Artist',
              using: {
                foreign_key_constraint_on: 'ArtistId',
              },
            },
          ],
          array_relationships: [
            {
              name: 'Tracks',
              using: {
                foreign_key_constraint_on: {
                  column: 'AlbumId',
                  table: ['Track'],
                },
              },
            },
          ],
          select_permissions: [
            {
              role: 'a',
              permission: {
                columns: ['AlbumId', 'Title', 'ArtistId'],
                filter: {},
              },
              comment: '',
            },
            {
              role: 'v',
              permission: {
                columns: [],
                filter: {
                  Artist: {
                    Albums: {
                      Name: {
                        _eq: '',
                      },
                    },
                  },
                },
              },
              comment: '',
            },
          ],
        },
        {
          table: ['Artist'],
          array_relationships: [
            {
              name: 'Albums',
              using: {
                foreign_key_constraint_on: {
                  column: 'ArtistId',
                  table: ['Album'],
                },
              },
            },
          ],
        },
        {
          table: ['Customer'],
          object_relationships: [
            {
              name: 'Employee',
              using: {
                foreign_key_constraint_on: 'SupportRepId',
              },
            },
          ],
          array_relationships: [
            {
              name: 'Invoices',
              using: {
                foreign_key_constraint_on: {
                  column: 'CustomerId',
                  table: ['Invoice'],
                },
              },
            },
          ],
        },
        {
          table: ['Employee'],
          object_relationships: [
            {
              name: 'Employee',
              using: {
                foreign_key_constraint_on: 'ReportsTo',
              },
            },
          ],
          array_relationships: [
            {
              name: 'Customers',
              using: {
                foreign_key_constraint_on: {
                  column: 'SupportRepId',
                  table: ['Customer'],
                },
              },
            },
            {
              name: 'Employees',
              using: {
                foreign_key_constraint_on: {
                  column: 'ReportsTo',
                  table: ['Employee'],
                },
              },
            },
          ],
        },
        {
          table: ['Genre'],
          array_relationships: [
            {
              name: 'Tracks',
              using: {
                foreign_key_constraint_on: {
                  column: 'GenreId',
                  table: ['Track'],
                },
              },
            },
          ],
        },
        {
          table: ['Invoice'],
          object_relationships: [
            {
              name: 'Customer',
              using: {
                foreign_key_constraint_on: 'CustomerId',
              },
            },
          ],
          array_relationships: [
            {
              name: 'InvoiceLines',
              using: {
                foreign_key_constraint_on: {
                  column: 'InvoiceId',
                  table: ['InvoiceLine'],
                },
              },
            },
          ],
        },
        {
          table: ['InvoiceLine'],
          object_relationships: [
            {
              name: 'Invoice',
              using: {
                foreign_key_constraint_on: 'InvoiceId',
              },
            },
            {
              name: 'Track',
              using: {
                foreign_key_constraint_on: 'TrackId',
              },
            },
          ],
        },
        {
          table: ['MediaType'],
          array_relationships: [
            {
              name: 'Tracks',
              using: {
                foreign_key_constraint_on: {
                  column: 'MediaTypeId',
                  table: ['Track'],
                },
              },
            },
          ],
        },
        {
          table: ['Playlist'],
          array_relationships: [
            {
              name: 'PlaylistTracks',
              using: {
                foreign_key_constraint_on: {
                  column: 'PlaylistId',
                  table: ['PlaylistTrack'],
                },
              },
            },
          ],
        },
        {
          table: ['PlaylistTrack'],
          object_relationships: [
            {
              name: 'Playlist',
              using: {
                foreign_key_constraint_on: 'PlaylistId',
              },
            },
            {
              name: 'Track',
              using: {
                foreign_key_constraint_on: 'TrackId',
              },
            },
          ],
        },
        {
          table: ['Track'],
          object_relationships: [
            {
              name: 'Album',
              using: {
                foreign_key_constraint_on: 'AlbumId',
              },
            },
            {
              name: 'Genre',
              using: {
                foreign_key_constraint_on: 'GenreId',
              },
            },
            {
              name: 'MediaType',
              using: {
                foreign_key_constraint_on: 'MediaTypeId',
              },
            },
          ],
          array_relationships: [
            {
              name: 'InvoiceLines',
              using: {
                foreign_key_constraint_on: {
                  column: 'TrackId',
                  table: ['InvoiceLine'],
                },
              },
            },
            {
              name: 'PlaylistTracks',
              using: {
                foreign_key_constraint_on: {
                  column: 'TrackId',
                  table: ['PlaylistTrack'],
                },
              },
            },
          ],
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
    relationships: [
      {
        name: 'Playlist',
        fromSource: 'MySQ',
        fromTable: ['PlaylistTrack'],
        relationshipType: 'Object',
        type: 'localRelationship',
        definition: {
          toTable: ['Playlist'],
          toColumns: ['PlaylistId'],
          fromTable: ['PlaylistTrack'],
          fromColumns: ['PlaylistId'],
          mapping: {
            PlaylistId: 'PlaylistId',
          },
        },
      },
      {
        name: 'Track',
        fromSource: 'MySQ',
        fromTable: ['PlaylistTrack'],
        relationshipType: 'Object',
        type: 'localRelationship',
        definition: {
          toTable: ['Track'],
          toColumns: ['TrackId'],
          fromTable: ['PlaylistTrack'],
          fromColumns: ['TrackId'],
          mapping: {
            TrackId: 'TrackId',
          },
        },
      },
    ],
    columns: [
      {
        name: 'PlaylistId',
        dataType: 'string',
        consoleDataType: 'string',
        nullable: false,
        isPrimaryKey: true,
        graphQLProperties: {
          name: 'PlaylistId',
          scalarType: 'Int',
          graphQLType: 'Int!',
        },
      },
      {
        name: 'TrackId',
        dataType: 'string',
        consoleDataType: 'string',
        nullable: false,
        isPrimaryKey: true,
        graphQLProperties: {
          name: 'TrackId',
          scalarType: 'Int',
          graphQLType: 'Int!',
        },
      },
    ],
  },
  {
    table: ['Track'],
    dataSource: {
      name: 'MySQ',
      kind: 'mysql8',
      tables: [
        {
          table: ['Album'],
          object_relationships: [
            {
              name: 'Artist',
              using: {
                foreign_key_constraint_on: 'ArtistId',
              },
            },
          ],
          array_relationships: [
            {
              name: 'Tracks',
              using: {
                foreign_key_constraint_on: {
                  column: 'AlbumId',
                  table: ['Track'],
                },
              },
            },
          ],
          select_permissions: [
            {
              role: 'a',
              permission: {
                columns: ['AlbumId', 'Title', 'ArtistId'],
                filter: {},
              },
              comment: '',
            },
            {
              role: 'v',
              permission: {
                columns: [],
                filter: {
                  Artist: {
                    Albums: {
                      Name: {
                        _eq: '',
                      },
                    },
                  },
                },
              },
              comment: '',
            },
          ],
        },
        {
          table: ['Artist'],
          array_relationships: [
            {
              name: 'Albums',
              using: {
                foreign_key_constraint_on: {
                  column: 'ArtistId',
                  table: ['Album'],
                },
              },
            },
          ],
        },
        {
          table: ['Customer'],
          object_relationships: [
            {
              name: 'Employee',
              using: {
                foreign_key_constraint_on: 'SupportRepId',
              },
            },
          ],
          array_relationships: [
            {
              name: 'Invoices',
              using: {
                foreign_key_constraint_on: {
                  column: 'CustomerId',
                  table: ['Invoice'],
                },
              },
            },
          ],
        },
        {
          table: ['Employee'],
          object_relationships: [
            {
              name: 'Employee',
              using: {
                foreign_key_constraint_on: 'ReportsTo',
              },
            },
          ],
          array_relationships: [
            {
              name: 'Customers',
              using: {
                foreign_key_constraint_on: {
                  column: 'SupportRepId',
                  table: ['Customer'],
                },
              },
            },
            {
              name: 'Employees',
              using: {
                foreign_key_constraint_on: {
                  column: 'ReportsTo',
                  table: ['Employee'],
                },
              },
            },
          ],
        },
        {
          table: ['Genre'],
          array_relationships: [
            {
              name: 'Tracks',
              using: {
                foreign_key_constraint_on: {
                  column: 'GenreId',
                  table: ['Track'],
                },
              },
            },
          ],
        },
        {
          table: ['Invoice'],
          object_relationships: [
            {
              name: 'Customer',
              using: {
                foreign_key_constraint_on: 'CustomerId',
              },
            },
          ],
          array_relationships: [
            {
              name: 'InvoiceLines',
              using: {
                foreign_key_constraint_on: {
                  column: 'InvoiceId',
                  table: ['InvoiceLine'],
                },
              },
            },
          ],
        },
        {
          table: ['InvoiceLine'],
          object_relationships: [
            {
              name: 'Invoice',
              using: {
                foreign_key_constraint_on: 'InvoiceId',
              },
            },
            {
              name: 'Track',
              using: {
                foreign_key_constraint_on: 'TrackId',
              },
            },
          ],
        },
        {
          table: ['MediaType'],
          array_relationships: [
            {
              name: 'Tracks',
              using: {
                foreign_key_constraint_on: {
                  column: 'MediaTypeId',
                  table: ['Track'],
                },
              },
            },
          ],
        },
        {
          table: ['Playlist'],
          array_relationships: [
            {
              name: 'PlaylistTracks',
              using: {
                foreign_key_constraint_on: {
                  column: 'PlaylistId',
                  table: ['PlaylistTrack'],
                },
              },
            },
          ],
        },
        {
          table: ['PlaylistTrack'],
          object_relationships: [
            {
              name: 'Playlist',
              using: {
                foreign_key_constraint_on: 'PlaylistId',
              },
            },
            {
              name: 'Track',
              using: {
                foreign_key_constraint_on: 'TrackId',
              },
            },
          ],
        },
        {
          table: ['Track'],
          object_relationships: [
            {
              name: 'Album',
              using: {
                foreign_key_constraint_on: 'AlbumId',
              },
            },
            {
              name: 'Genre',
              using: {
                foreign_key_constraint_on: 'GenreId',
              },
            },
            {
              name: 'MediaType',
              using: {
                foreign_key_constraint_on: 'MediaTypeId',
              },
            },
          ],
          array_relationships: [
            {
              name: 'InvoiceLines',
              using: {
                foreign_key_constraint_on: {
                  column: 'TrackId',
                  table: ['InvoiceLine'],
                },
              },
            },
            {
              name: 'PlaylistTracks',
              using: {
                foreign_key_constraint_on: {
                  column: 'TrackId',
                  table: ['PlaylistTrack'],
                },
              },
            },
          ],
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
    relationships: [
      {
        name: 'InvoiceLines',
        fromSource: 'MySQ',
        fromTable: ['Track'],
        relationshipType: 'Array',
        type: 'localRelationship',
        definition: {
          toTable: ['InvoiceLine'],
          toColumns: ['TrackId'],
          fromTable: ['Track'],
          fromColumns: ['TrackId'],
          mapping: {
            TrackId: 'TrackId',
          },
        },
      },
      {
        name: 'PlaylistTracks',
        fromSource: 'MySQ',
        fromTable: ['Track'],
        relationshipType: 'Array',
        type: 'localRelationship',
        definition: {
          toTable: ['PlaylistTrack'],
          toColumns: ['TrackId'],
          fromTable: ['Track'],
          fromColumns: ['TrackId'],
          mapping: {
            TrackId: 'TrackId',
          },
        },
      },
      {
        name: 'Album',
        fromSource: 'MySQ',
        fromTable: ['Track'],
        relationshipType: 'Object',
        type: 'localRelationship',
        definition: {
          toTable: ['Album'],
          toColumns: ['AlbumId'],
          fromTable: ['Track'],
          fromColumns: ['AlbumId'],
          mapping: {
            AlbumId: 'AlbumId',
          },
        },
      },
      {
        name: 'Genre',
        fromSource: 'MySQ',
        fromTable: ['Track'],
        relationshipType: 'Object',
        type: 'localRelationship',
        definition: {
          toTable: ['Genre'],
          toColumns: ['GenreId'],
          fromTable: ['Track'],
          fromColumns: ['GenreId'],
          mapping: {
            GenreId: 'GenreId',
          },
        },
      },
      {
        name: 'MediaType',
        fromSource: 'MySQ',
        fromTable: ['Track'],
        relationshipType: 'Object',
        type: 'localRelationship',
        definition: {
          toTable: ['MediaType'],
          toColumns: ['MediaTypeId'],
          fromTable: ['Track'],
          fromColumns: ['MediaTypeId'],
          mapping: {
            MediaTypeId: 'MediaTypeId',
          },
        },
      },
    ],
    columns: [
      {
        name: 'TrackId',
        dataType: 'string',
        consoleDataType: 'string',
        nullable: false,
        isPrimaryKey: true,
        graphQLProperties: {
          name: 'TrackId',
          scalarType: 'Int',
          graphQLType: 'Int!',
        },
      },
      {
        name: 'Name',
        dataType: 'string',
        consoleDataType: 'string',
        nullable: false,
        isPrimaryKey: false,
        graphQLProperties: {
          name: 'Name',
          scalarType: 'String',
          graphQLType: 'String!',
        },
      },
      {
        name: 'AlbumId',
        dataType: 'string',
        consoleDataType: 'string',
        nullable: true,
        isPrimaryKey: false,
        graphQLProperties: {
          name: 'AlbumId',
          scalarType: 'Int',
          graphQLType: 'Int',
        },
      },
      {
        name: 'MediaTypeId',
        dataType: 'string',
        consoleDataType: 'string',
        nullable: false,
        isPrimaryKey: false,
        graphQLProperties: {
          name: 'MediaTypeId',
          scalarType: 'Int',
          graphQLType: 'Int!',
        },
      },
      {
        name: 'GenreId',
        dataType: 'string',
        consoleDataType: 'string',
        nullable: true,
        isPrimaryKey: false,
        graphQLProperties: {
          name: 'GenreId',
          scalarType: 'Int',
          graphQLType: 'Int',
        },
      },
      {
        name: 'Composer',
        dataType: 'string',
        consoleDataType: 'string',
        nullable: true,
        isPrimaryKey: false,
        graphQLProperties: {
          name: 'Composer',
          scalarType: 'String',
          graphQLType: 'String',
        },
      },
      {
        name: 'Milliseconds',
        dataType: 'string',
        consoleDataType: 'string',
        nullable: false,
        isPrimaryKey: false,
        graphQLProperties: {
          name: 'Milliseconds',
          scalarType: 'Int',
          graphQLType: 'Int!',
        },
      },
      {
        name: 'Bytes',
        dataType: 'string',
        consoleDataType: 'string',
        nullable: true,
        isPrimaryKey: false,
        graphQLProperties: {
          name: 'Bytes',
          scalarType: 'Int',
          graphQLType: 'Int',
        },
      },
      {
        name: 'UnitPrice',
        dataType: 'number',
        consoleDataType: 'string',
        nullable: false,
        isPrimaryKey: false,
        graphQLProperties: {
          name: 'UnitPrice',
          scalarType: 'Float',
          graphQLType: 'Float!',
        },
      },
    ],
  },
] as Tables;
