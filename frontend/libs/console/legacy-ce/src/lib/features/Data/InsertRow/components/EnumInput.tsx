import { useImperativeHandle, useState } from 'react';
import { Select } from '@hasura/shared/ui';

export type EnumInputProps = {
  name?: string;
  /** the allowed values, from the referenced enum table */
  values: string[];
  disabled?: boolean;
  initialValue?: unknown;
  /**
   * Exposes the selected `value`, so the row can read and clear it like a
   * text input.
   */
  ref?: React.Ref<unknown>;
  onValueChange: (e: { target: { value: string } }) => void;
  onCheckValueRadio: () => void;
};

export const EnumInput = ({
  name,
  values,
  disabled,
  initialValue,
  ref,
  onValueChange,
  onCheckValueRadio,
}: EnumInputProps) => {
  const [value, setValue] = useState(
    initialValue === undefined || initialValue === null
      ? ''
      : String(initialValue),
  );

  useImperativeHandle(
    ref,
    () => ({
      get value() {
        return value;
      },
      set value(newValue: string) {
        setValue(newValue);
      },
    }),
    [value],
  );

  // keep a current value that is no longer in the enum table selectable
  const options = (
    value && !values.includes(value) ? [value, ...values] : values
  ).map((option) => ({ value: option, label: option }));

  return (
    <Select
      name={name}
      value={value}
      // the column's type placeholder doesn't apply to a list of values
      placeholder="Select a value"
      disabled={disabled}
      options={options}
      full
      onChange={(newValue) => {
        setValue(newValue);
        onValueChange({ target: { value: newValue } });
        onCheckValueRadio();
      }}
    />
  );
};
