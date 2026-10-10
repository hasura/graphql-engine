import React from 'react';
import {
  REACT_SELECT_FILTER_PROPS,
  ReactSelectField,
  ReactSelectFieldProps,
} from '@hasura/shared/ui';
import { mapItemsToSourceOptions } from './SourcePicker.utils';
import { SourceSelectorItem } from './SourcePicker.types';

type SourcePickerProps = Omit<ReactSelectFieldProps<any>, 'options'> & {
  items: SourceSelectorItem[];
};

export const SourcePicker: React.FC<SourcePickerProps> = ({
  items,
  label,
  name,
  disabled,
}) => {
  const sourceOptions = mapItemsToSourceOptions(
    items,
  ) as ReactSelectFieldProps['options'];

  return (
    <ReactSelectField
      label={label}
      name={name}
      options={sourceOptions}
      disabled={disabled}
      selectProps={REACT_SELECT_FILTER_PROPS}
    />
  );
};
