import { ReactNode } from 'react';
import { FaPlug, FaTable } from 'react-icons/fa';
import { Table } from '../../../../hasura-metadata-types';
import { MultiSelectItem } from '../../../../../new-components/Form';
import { getTableDisplayName } from '../../../utils/helpers';
import { SourceSelectorItem } from './SourcePicker.types';
import {
  isSchemaTable,
  isDatasetTable,
  isGDCTable,
} from '../../../../DataSource/utils';

type SourcePickerLabelProps = {
  prefix: ReactNode;
  tableName: string;
};

export const SourcePickerLabel = ({
  prefix,
  tableName,
}: SourcePickerLabelProps) => (
  <>
    <FaTable fill="fill-slate-900" />
    <span className="text-muted ml-1.5">
      {prefix}
      <span className="text-slate-900 ml-1.5">{tableName}</span>
    </span>
  </>
);

export const RemoteSchemaSourcePickerLabel = ({
  remoteSchemaName,
}: {
  remoteSchemaName: string;
}) => (
  <>
    <FaPlug fill="fill-slate-900" />
    <span className="text-slate-900 ml-1.5">{remoteSchemaName}</span>
  </>
);

export type TableAttributes = {
  sourceName: string;
  table: Table;
};

export const getTableLabel = ({
  sourceName,
  table,
}: TableAttributes): MultiSelectItem['label'] => {
  if (isSchemaTable(table)) {
    return (
      <SourcePickerLabel
        prefix={`${sourceName} / ${table.schema} /`}
        tableName={table.name}
      />
    );
  }

  if (isDatasetTable(table)) {
    return (
      <SourcePickerLabel
        prefix={`${sourceName} / ${table.dataset} /`}
        tableName={table.name}
      />
    );
  }

  if (isGDCTable(table)) {
    return (
      <SourcePickerLabel
        prefix={`${sourceName} /`}
        tableName={getTableDisplayName(table)}
      />
    );
  }

  return '';
};

export const mapItemsToSourceOptions = (
  items: SourceSelectorItem[]
): MultiSelectItem[] => {
  return items.map(item => {
    const value = item.value;

    let label: MultiSelectItem['label'] = '';
    if (item.type === 'table') {
      label = getTableLabel({
        sourceName: item.value.dataSourceName,
        table: item.value.table,
      });
    }

    if (item.type === 'remoteSchema') {
      label = (
        <RemoteSchemaSourcePickerLabel
          remoteSchemaName={item.value.remoteSchemaName}
        />
      );
    }

    return {
      value,
      label,
    };
  });
};
