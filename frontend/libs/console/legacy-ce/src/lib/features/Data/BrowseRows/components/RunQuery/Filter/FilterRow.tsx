import {
  getDialogPortalTarget,
  IconButton,
  InputField,
  ReactSelectField,
  ReactSelectOptionType,
} from '@hasura/shared/ui';
import { useEffect } from 'react';
import { useFormContext } from 'react-hook-form';
import { FaTimesCircle } from 'react-icons/fa';
import { Flex } from '@radix-ui/themes';
import { createFilter } from 'react-select';

type FilterRowProps = {
  columnOptions: ReactSelectOptionType[];
  onRemove: () => void;
  operatorOptions: ReactSelectOptionType[];
  name: string;
  defaultValues?: Record<string, string>;
  disabled?: boolean;
};

export const FilterRow = ({
  columnOptions,
  onRemove,
  operatorOptions,
  name,
  defaultValues,
  disabled,
}: FilterRowProps) => {
  const { setValue, watch } = useFormContext();
  const localOperator = watch(`${name}.operator`);
  const localValue = watch(`${name}.value`);

  /**
   * Set operator to first operator if it is empty
   */
  useEffect(() => {
    if (!localOperator && operatorOptions[0]?.value) {
      setValue(`${name}.operator`, operatorOptions[0].value);
    }
  }, [localOperator, operatorOptions, setValue, name]);

  /**
   * Set the default value into the input field depending on the operator type
   */
  useEffect(() => {
    if (localOperator && !localValue && defaultValues?.[localOperator]) {
      setValue(`${name}.value`, defaultValues?.[localOperator]);
    }
  }, [localOperator, localValue, defaultValues, setValue, name]);

  const defaultSelectProps = {
    filterOption: createFilter({
      ignoreCase: true,
      matchFrom: 'any',
    }),
    menuPortalTarget: getDialogPortalTarget(),
    disabled,
  };

  return (
    <Flex data-testid={`${name}-filter-row`} align="center" gap="2">
      <div className="sm:w-4/12">
        <ReactSelectField
          name={`${name}.column`}
          options={columnOptions}
          placeholder="Select a column"
          data-test={`${name}.column`}
          dataTest={`${name}.column`}
          noErrorPlaceholder
          selectProps={defaultSelectProps}
        />
      </div>

      <div className="sm:w-4/12">
        <ReactSelectField
          name={`${name}.operator`}
          options={operatorOptions}
          placeholder="Select an operator"
          data-test={`${name}.operator`}
          noErrorPlaceholder
          selectProps={defaultSelectProps}
        />
      </div>

      <div className="sm:w-4/12">
        <InputField
          name={`${name}.value`}
          fieldProps={{ placeholder: '-- value --' }}
          data-test={`${name}.value`}
          noErrorPlaceholder
        />
      </div>

      <IconButton
        color="indigo"
        variant="ghost"
        radius="full"
        disabled={false}
        onClick={onRemove}
        data-testid={`${name}.remove`}
      >
        <FaTimesCircle />
      </IconButton>
    </Flex>
  );
};
