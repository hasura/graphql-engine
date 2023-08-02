import { Table } from '../../../hasura-metadata-types';
import React, { useEffect } from 'react';
import { useFormContext, useWatch } from 'react-hook-form';
import { FaArrowAltCircleLeft, FaArrowAltCircleRight } from 'react-icons/fa';
import Skeleton from 'react-loading-skeleton';
import { useMetadata, MetadataSelectors } from '../../../hasura-metadata-api';
import { SourcePicker } from './SourcePicker/SourcePicker';
import { SourceSelectorItem } from './SourcePicker/SourcePicker.types';
import { mapItemsToSourceOptions } from './SourcePicker/SourcePicker.utils';
import { TablePickerProps } from './TablePicker.types';
import {
  getDefaultSourceSelectorItem,
  mapMetadataSourceToSelectorItems,
} from './TablePicker.utils';

export const TablePicker: React.VFC<TablePickerProps> = ({
  type,
  disabled = false,
  isCurrentSource = false,
  filterDataSource,
}) => {
  const { setValue } = useFormContext();

  const currentDataSourceName = useWatch<Record<string, string | undefined>>({
    name: `${type}.value.dataSourceName`,
  });

  const currentTable = useWatch<Record<string, Table>>({
    name: `${type}.value.table`,
  });

  const { data: metadataSources } = useMetadata(MetadataSelectors.getSources());

  const items: SourceSelectorItem[] = mapMetadataSourceToSelectorItems(
    metadataSources || [],
    filterDataSource
  );

  const defaultValue = isCurrentSource
    ? getDefaultSourceSelectorItem({
        sourceSelectorItems: items,
        dataSourceName: currentDataSourceName,
        table: currentTable,
      })
    : undefined;

  useEffect(() => {
    if (defaultValue) {
      setValue(type, mapItemsToSourceOptions([defaultValue])[0]);
    }
  }, [defaultValue?.type]);

  if (!metadataSources) return <Skeleton count={5} height={20} />;

  return (
    <div className="h-full">
      <div className="my-2">
        <SourcePicker
          name={type}
          items={items}
          label={type === 'fromSource' ? 'From Source' : 'To Reference'}
          labelIcon={
            type === 'fromSource' ? (
              <FaArrowAltCircleRight className="fill-emerald-700 ml-1.5" />
            ) : (
              <FaArrowAltCircleLeft className="fill-violet-700 ml-1.5" />
            )
          }
          disabled={disabled}
        />
      </div>
    </div>
  );
};
