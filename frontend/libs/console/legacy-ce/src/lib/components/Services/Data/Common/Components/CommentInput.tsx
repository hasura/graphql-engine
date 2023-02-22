import { SelectInputSplitField } from '../../../../Common/SelectInputSplitField/SelectInputSplitField';
import React, { ChangeEvent, useCallback } from 'react';

export type CommentProps = {
  value: string | null;
  defaultComment: string;
  dataTest?: string;
  onChange: (comment: string | null) => void;
};

const selectOptions = [
  { label: 'Value', value: 'Value' },
  { label: 'Disabled', value: 'Disabled' },
];

export const CommentInput = ({
  value,
  defaultComment,
  onChange,
  dataTest = '',
}: CommentProps) => {
  const inputValue = value ?? '';
  const selectValue = value === '' ? 'Disabled' : 'Value';
  const placeholder = selectValue === 'Value' ? defaultComment : '';

  const inputOnChange = useCallback(
    (e: ChangeEvent<HTMLInputElement>) => {
      if (selectValue === 'Disabled') {
        onChange('');
      } else {
        onChange(e.target.value === '' ? null : e.target.value);
      }
    },
    [selectValue, onChange]
  );

  const selectOnChange = useCallback(
    (e: ChangeEvent<HTMLSelectElement>) => {
      if (e.target.value === selectValue) return; // No change

      const newComment = e.target.value === 'Disabled' ? '' : null;
      onChange(newComment);
    },
    [selectValue, onChange]
  );

  return (
    <SelectInputSplitField
      dataTest={dataTest}
      inputValue={inputValue}
      inputOnChange={inputOnChange}
      inputDisabled={selectValue === 'Disabled'}
      selectOptions={selectOptions}
      selectValue={selectValue}
      selectOnChange={selectOnChange}
      placeholder={placeholder}
    />
  );
};
