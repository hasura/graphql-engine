import { DatasetTable, SchemaTable } from '../../../../DataSource/utils';
import {
  getTableLabel,
  SourcePickerLabel,
  TableAttributes,
  mapItemsToSourceOptions,
  RemoteSchemaSourcePickerLabel,
} from './SourcePicker.utils';
import type { GDCTable } from '../../../../../features/DataSource';
import type { MultiSelectItem } from '../../../../../new-components/Form';
import { SourceSelectorItem } from './SourcePicker.types';

describe('getTableLabel', () => {
  describe('when table is GDC table', () => {
    it('returns the label', () => {
      const table: GDCTable = ['Album'];
      const input: TableAttributes = {
        sourceName: 'Chinook',
        table,
      };

      expect(getTableLabel(input)).toEqual(
        <SourcePickerLabel prefix="Chinook /" tableName="Album" />
      );
    });
  });

  describe('when table is dataset table', () => {
    it('returns the label', () => {
      const table: DatasetTable = {
        dataset: 'ds',
        name: 'tbl',
      };
      const input: TableAttributes = {
        sourceName: 'Chinook',
        table,
      };

      expect(getTableLabel(input)).toEqual(
        <SourcePickerLabel prefix="Chinook / ds /" tableName="tbl" />
      );
    });
  });

  describe('when table is dataset table', () => {
    it('returns the label', () => {
      const table: DatasetTable = {
        dataset: 'ds',
        name: 'tbl',
      };
      const input: TableAttributes = {
        sourceName: 'Chinook',
        table,
      };

      expect(getTableLabel(input)).toEqual(
        <SourcePickerLabel prefix="Chinook / ds /" tableName="tbl" />
      );
    });
  });

  describe('when table is schema table', () => {
    it('returns the label', () => {
      const table: SchemaTable = {
        schema: 'public',
        name: 'tbl',
      };
      const input: TableAttributes = {
        sourceName: 'Chinook',
        table,
      };

      expect(getTableLabel(input)).toEqual(
        <SourcePickerLabel prefix="Chinook / public /" tableName="tbl" />
      );
    });
  });
});

describe('mapItemsToSourceOptions', () => {
  it('returns the source options', () => {
    const items: SourceSelectorItem[] = [
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
          remoteSchemaName: 'remote schema name',
        },
      },
    ];

    const expected: MultiSelectItem[] = [
      {
        label: <SourcePickerLabel prefix="Chinook /" tableName="Album" />,
        value: {
          dataSourceName: 'Chinook',
          table: ['Album'],
        },
      },
      {
        label: (
          <RemoteSchemaSourcePickerLabel remoteSchemaName="remote schema name" />
        ),
        value: {
          remoteSchemaName: 'remote schema name',
        },
      },
    ];

    expect(mapItemsToSourceOptions(items)).toEqual(expected);
  });
});
