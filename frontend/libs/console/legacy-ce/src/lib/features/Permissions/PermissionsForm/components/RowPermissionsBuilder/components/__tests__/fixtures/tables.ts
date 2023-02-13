import { Tables } from '../../types';

export const tables: Tables = [
  {
    table: { schema: 'public', name: 'Label' },
    columns: [
      { name: 'id', type: 'Int_comparison_exp' },
      { name: 'name', type: 'String_comparison_exp' },
    ],
    relationships: [],
  },
  {
    table: ['Artist'],
    columns: [
      { name: 'id', type: 'number_SQLite_comparison_exp' },
      { name: 'name', type: 'string_SQLite_comparison_exp' },
      { name: 'surname', type: 'string_SQLite_comparison_exp' },
    ],
    relationships: [],
  },
  {
    table: ['Album'],
    columns: [
      { name: 'id', type: 'number_SQLite_comparison_exp' },
      { name: 'title', type: 'string_SQLite_comparison_exp' },
    ],
    relationships: [
      {
        name: 'Author',
        table: { schema: 'public', name: 'Artist' },
        type: 'object',
      },
      {
        name: 'Label',
        table: { schema: 'public', name: 'Label' },
        type: 'object',
      },
    ],
  },
  {
    table: ['Customer'],
    columns: [],
    relationships: [],
  },
  {
    table: { dataset: 'bigquery_sample', name: 'sample_table' },
    columns: [
      { name: 'Series_reference', type: 'String_BigQuery_comparison_exp' },
      { name: 'Period', type: 'Float_BigQuery_comparison_exp' },
      { name: 'Data_value', type: 'Float_BigQuery_comparison_exp' },
      { name: 'Suppressed', type: 'Boolean_BigQuery_comparison_exp' },
      { name: 'STATUS', type: 'String_BigQuery_comparison_exp' },
      { name: 'UNITS', type: 'String_BigQuery_comparison_exp' },
      { name: 'Magnitude', type: 'Int_comparison_exp' },
      { name: 'Subject', type: 'String_BigQuery_comparison_exp' },
      { name: 'Group', type: 'String_BigQuery_comparison_exp' },
      { name: 'Series_title_1', type: 'String_BigQuery_comparison_exp' },
      { name: 'Series_title_2', type: 'String_BigQuery_comparison_exp' },
      { name: 'Series_title_3', type: 'String_BigQuery_comparison_exp' },
      { name: 'Series_title_4', type: 'String_BigQuery_comparison_exp' },
      { name: 'Series_title_5', type: 'String_BigQuery_comparison_exp' },
    ],
    relationships: [],
  },
];
