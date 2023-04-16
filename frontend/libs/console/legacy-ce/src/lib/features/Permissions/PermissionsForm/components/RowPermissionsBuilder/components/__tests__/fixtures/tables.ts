import { Tables } from '../../types';

export const tables: Tables = [
  {
    table: { schema: 'public', name: 'Label' },
    dataSource: { name: 'default', kind: 'postgres' },
    columns: [
      {
        name: 'id',
        dataType: 'Int',
      },
      {
        name: 'name',
        dataType: 'String',
      },
      {
        name: 'doc',
        dataType: 'jsonb',
      },
    ],
    relationships: [],
  },
  {
    table: ['Artist'],
    dataSource: { name: 'SQLite', kind: 'SQLite' },
    columns: [
      {
        name: 'id',
        dataType: 'number_SQLite',
      },
      {
        name: 'name',
        dataType: 'string_SQLite',
      },
      {
        name: 'surname',
        dataType: 'string_SQLite',
      },
    ],
    relationships: [],
  },
  {
    table: ['Album'],
    dataSource: { name: 'SQLite', kind: 'SQLite' },
    columns: [
      {
        name: 'id',
        dataType: 'number_SQLite',
      },
      {
        name: 'title',
        dataType: 'string_SQLite',
      },
    ],
    relationships: [
      {
        name: 'Author',
        relationshipType: 'Object',
        type: 'localRelationship',
        fromSource: 'Album',
        fromTable: ['Album'],
        definition: {
          mapping: {
            author_id: 'id',
          },
          toTable: ['Artist'],
        },
      },
      {
        name: 'Label',
        relationshipType: 'Object',
        type: 'localRelationship',
        fromSource: 'Album',
        fromTable: ['Album'],
        definition: {
          mapping: {
            label_id: 'id',
          },
          toTable: { schema: 'public', name: 'Label' },
        },
      },
    ],
  },
  {
    table: ['Customer'],
    dataSource: { name: 'SQLite', kind: 'SQLite' },
    columns: [],
    relationships: [],
  },
  {
    table: { dataset: 'bigquery_sample', name: 'sample_table' },
    dataSource: { name: 'BigQuery', kind: 'bigquery' },
    columns: [
      {
        name: 'Series_reference',
        dataType: 'String_BigQuery',
      },
      {
        name: 'Period',
        dataType: 'Float_BigQuery',
      },
      {
        name: 'Data_value',
        dataType: 'Float_BigQuery',
      },
      {
        name: 'Suppressed',
        dataType: 'Boolean_BigQuery',
      },
      {
        name: 'STATUS',
        dataType: 'String_BigQuery',
      },
      {
        name: 'UNITS',
        dataType: 'String_BigQuery',
      },
      {
        name: 'Magnitude',
        dataType: 'Int',
      },
      {
        name: 'Subject',
        dataType: 'String_BigQuery',
      },
      {
        name: 'Group',
        dataType: 'String_BigQuery',
      },
      {
        name: 'Series_title_1',
        dataType: 'String_BigQuery',
      },
      {
        name: 'Series_title_2',
        dataType: 'String_BigQuery',
      },
      {
        name: 'Series_title_3',
        dataType: 'String_BigQuery',
      },
      {
        name: 'Series_title_4',
        dataType: 'String_BigQuery',
      },
      {
        name: 'Series_title_5',
        dataType: 'String_BigQuery',
      },
    ],
    relationships: [],
  },
];

export const tableWithGeolocationSupport = [
  {
    table: { name: 'user_location', schema: 'public' },
    dataSource: {
      name: 'postgis',
      kind: 'postgres',
      tables: [
        {
          table: { name: 'user_location', schema: 'public' },
          select_permissions: [
            {
              role: 'new',
              permission: {
                columns: [],
                filter: {
                  location: {
                    _st_d_within: {
                      distance: 100000,
                      from: { coordinates: [1.4, 2.5], type: 'Point' },
                      use_spheroid: false,
                    },
                  },
                },
              },
            },
          ],
        },
      ],
      configuration: {
        connection_info: {
          database_url: { from_env: 'DB2' },
          isolation_level: 'read-committed',
          use_prepared_statements: false,
        },
      },
    },
    relationships: [],
    columns: [
      {
        name: 'user_id',
        dataType: 'integer',
        consoleDataType: 'number',
        nullable: false,
        isPrimaryKey: true,
        graphQLProperties: { name: 'user_id', scalarType: 'Int' },
      },
      {
        name: 'location',
        dataType: 'USER-DEFINED',
        consoleDataType: 'string',
        nullable: true,
        isPrimaryKey: false,
        graphQLProperties: { name: 'location', scalarType: 'geography' },
      },
      {
        name: 'topoelement',
        dataType: 'ARRAY',
        consoleDataType: 'string',
        nullable: true,
        isPrimaryKey: false,
        graphQLProperties: { name: 'topoelement', scalarType: '_int4' },
      },
      {
        name: 'norm_addy',
        dataType: 'USER-DEFINED',
        consoleDataType: 'string',
        nullable: true,
        isPrimaryKey: false,
        graphQLProperties: {
          name: 'norm_addy',
          scalarType: 'norm_addy_scalar',
        },
      },
      {
        name: 'valid_topo',
        dataType: 'USER-DEFINED',
        consoleDataType: 'string',
        nullable: true,
        isPrimaryKey: false,
        graphQLProperties: {
          name: 'valid_topo',
          scalarType: 'validatetopology_returntype_scalar',
        },
      },
    ],
  },
];
