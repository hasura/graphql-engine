import { Table } from '@hasura/shared/types';
import React, { useEffect, useRef } from 'react';
import { useFormContext, useWatch } from 'react-hook-form';
import { FaArrowAltCircleLeft, FaArrowAltCircleRight } from 'react-icons/fa';
import { useMetadata } from '@hasura/metadata/api';
import { SourcePicker } from './SourcePicker/SourcePicker';
import { SourceSelectorItem } from './SourcePicker/SourcePicker.types';
import { mapItemsToSourceOptions } from './SourcePicker/SourcePicker.utils';
import { TablePickerProps } from './TablePicker.types';
import {
  getDefaultSourceSelectorItem,
  mapMetadataSourceToSelectorItems,
} from './TablePicker.utils';
import { SkeletonList } from '@hasura/shared/ui';
import { MetadataSelectors } from '@hasura/metadata/helpers';

export const TablePicker: React.FC<TablePickerProps> = ({
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
    filterDataSource,
  );

  const defaultValue = isCurrentSource
    ? getDefaultSourceSelectorItem({
        sourceSelectorItems: items,
        dataSourceName: currentDataSourceName,
        table: currentTable,
      })
    : undefined;

  // `defaultValue` is recomputed from the watched form values on every render,
  // so we intentionally only re-run the apply effect when its resolved `.type`
  // changes (depending on the object itself would re-run on every keystroke and
  // fight the user's own selection, since `setValue` updates the watched
  // value). We keep the latest `defaultValue` in a ref — written from an effect
  // so no ref is touched during render — and read it when applying.
  const defaultValueRef = useRef(defaultValue);
  useEffect(() => {
    defaultValueRef.current = defaultValue;
  });

  useEffect(() => {
    const latestDefaultValue = defaultValueRef.current;
    if (latestDefaultValue) {
      setValue(type, mapItemsToSourceOptions([latestDefaultValue])[0]);
    }
  }, [defaultValue?.type, setValue, type]);

  if (!metadataSources) return <SkeletonList count={5} />;

  return (
    <div className="h-full">
      <div className="my-2">
        <SourcePicker
          name={type}
          items={items}
          label={type === 'fromSource' ? 'From Source' : 'To Reference'}
          labelIcon={
            type === 'fromSource' ? (
              <FaArrowAltCircleRight className="fill-emerald-700 h-4 w-4 ml-1.5" />
            ) : (
              <FaArrowAltCircleLeft className="fill-violet-700 h-4 w-4 ml-1.5" />
            )
          }
          disabled={disabled}
        />
      </div>
    </div>
  );
};
