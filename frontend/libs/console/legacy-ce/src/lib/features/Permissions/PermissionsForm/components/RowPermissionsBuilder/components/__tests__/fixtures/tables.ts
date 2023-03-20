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
