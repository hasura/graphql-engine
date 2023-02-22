import React, { ReactNode } from 'react';
import {
  AdvancedSelectField,
  AdvancedSelectProps,
} from '../../../../../new-components/Form';
import { mapItemsToSourceOptions } from './SourcePicker.utils';
import { SourceSelectorItem } from './SourcePicker.types';

type SourcePickerProps = {
  items: SourceSelectorItem[];
  name: string;
  label: string | ReactNode;
  defaultValue?: SourceSelectorItem;
  disabled?: boolean;
  onChange?: AdvancedSelectProps['onChange'];
};

export const SourcePicker: React.VFC<SourcePickerProps> = ({
  items,
  label,
  name,
  disabled,
  onChange,
}) => {
  const sourceOptions = mapItemsToSourceOptions(items);

  return (
    <>
      {!!label && (
        <div className="flex items-center gap-2 text-muted mb-xs">
          <label className="font-semibold flex items-center">{label}</label>
        </div>
      )}
      <AdvancedSelectField
        name={name}
        options={sourceOptions}
        disabled={disabled}
        onChange={onChange}
      />
    </>
  );
};
