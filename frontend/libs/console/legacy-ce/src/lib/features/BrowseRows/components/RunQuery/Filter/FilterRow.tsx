import { SelectItem } from '../../../../../components/Common/SelectInputSplitField/SelectInputSplitField';
import { Button } from '../../../../../new-components/Button';
import { InputField, Select } from '../../../../../new-components/Form';
import React, { useEffect } from 'react';
import { useFormContext } from 'react-hook-form';
import { FaTimes } from 'react-icons/fa';

export type OperatorItem = SelectItem & { defaultValue?: string };

type FilterRowProps = {
  columnOptions: SelectItem[];
  onRemove: () => void;
  operatorOptions: SelectItem[];
  name: string;
  defaultValues?: Record<string, string>;
};

export const FilterRow = ({
  columnOptions,
  onRemove,
  operatorOptions,
  name,
  defaultValues,
}: FilterRowProps) => {
  const { setValue, watch } = useFormContext();
  const localOperator = watch(`${name}.operator`);
  const localValue = watch(`${name}.value`);

  /**
   * Set the default value into the input field depending on the operator type
   */
  useEffect(() => {
    if (localOperator && !localValue && defaultValues?.[localOperator]) {
      setValue(`${name}.value`, defaultValues?.[localOperator]);
    }
  }, [localOperator, localValue, defaultValues, setValue, name]);

  return (
    <div className="flex space-x-4" data-testid={`${name}-filter-row`}>
      <Select
        name={`${name}.column`}
        options={columnOptions}
        placeholder="Select a column"
        data-test={`${name}.column`}
        dataTest={`${name}.column`}
      />

      <Select
        name={`${name}.operator`}
        options={operatorOptions}
        placeholder="Select an operator"
        data-test={`${name}.operator`}
      />

      <InputField
        name={`${name}.value`}
        placeholder="-- value --"
        data-test={`${name}.value`}
      />

      <Button
        mode="default"
        icon={<FaTimes />}
        className="mr-1"
        disabled={false}
        onClick={onRemove}
        data-testid={`${name}.remove`}
      />
    </div>
  );
};
