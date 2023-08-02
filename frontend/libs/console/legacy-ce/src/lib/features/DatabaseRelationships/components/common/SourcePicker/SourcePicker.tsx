import React, { ReactNode } from 'react';
import {
  SelectField,
  SelectFieldProps,
} from '../../../../../new-components/Form';
import { mapItemsToSourceOptions } from './SourcePicker.utils';
import { SourceSelectorItem } from './SourcePicker.types';

type SourcePickerProps = Omit<SelectFieldProps, 'options'> & {
  items: SourceSelectorItem[];
};

export const SourcePicker: React.VFC<SourcePickerProps> = ({
  items,
  label,
  name,
  disabled,
}) => {
  const sourceOptions = mapItemsToSourceOptions(items);

  return (
    <SelectField
      label={label}
      name={name}
      options={sourceOptions}
      disabled={disabled}
    />
  );
};
