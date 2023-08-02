import {
  filterMetadataSources,
  mapMetadataSourceToSelectorItems,
  getDefaultSourceSelectorItem,
} from './TablePicker.utils';
import type { Source } from '../../../../features/hasura-metadata-types';
import { SourceSelectorItem } from './SourcePicker/SourcePicker.types';

const sources: Source[] = [
  {
    name: 'BigQuery',
    kind: 'bigquery',
    tables: [
      {
        table: {
          dataset: 'cross_project_testing_europe_west_2',
          name: 'dogs',
        },
      },
    ],
    configuration: {},
  },
  {
    name: 'Chinook',
    kind: 'Chinook',
    tables: [
      {
        table: ['Album'],
      },
      {
        table: ['Artist'],
      },
      { table: ['Genre'] },
      { table: ['Track'] },
    ],
    configuration: {},
  },
];

describe('filterMetadataSources', () => {
  it('filters the sources', () => {
    const filterDataSource = 'Chinook';
    const expected: Source[] = [sources[1]];

    expect(filterMetadataSources(sources, filterDataSource)).toEqual(expected);
  });

  it('returns the same list if no filter is provided', () => {
    expect(filterMetadataSources(sources, undefined)).toEqual(sources);
  });
});

describe('mapMetadataSourceToSelectorItems', () => {
  it('maps data', () => {
    const filterDataSource = 'Chinook';
    const expected: SourceSelectorItem[] = [
      { type: 'table', value: { table: ['Album'], dataSourceName: 'Chinook' } },
      {
        type: 'table',
        value: { table: ['Artist'], dataSourceName: 'Chinook' },
      },
      { type: 'table', value: { table: ['Genre'], dataSourceName: 'Chinook' } },
      { type: 'table', value: { table: ['Track'], dataSourceName: 'Chinook' } },
    ];

    expect(mapMetadataSourceToSelectorItems(sources, filterDataSource)).toEqual(
      expected
    );
  });
});

describe('getDefaultSourceSelectorItem', () => {
  it('returns the default value', () => {
    const sourceSelectorItems: SourceSelectorItem[] = [
      {
        type: 'table',
        value: {
          dataSourceName: 'Chinook',
          table: ['Album'],
        },
      },
      {
        type: 'remoteSchema',
        value: {
          remoteSchemaName: 'remote schema',
        },
      },
    ];

    const expected: SourceSelectorItem = sourceSelectorItems[0];
    expect(
      getDefaultSourceSelectorItem({
        sourceSelectorItems,
        dataSourceName: 'Chinook',
        table: ['Album'],
      })
    ).toEqual(expected);
  });
});
