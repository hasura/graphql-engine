import clsx from 'clsx';
import {
  CardedTable,
  InputField,
  Button,
  SelectField,
  SwitchField,
} from '@hasura/shared/ui';

import { useFieldArray, useFormContext } from 'react-hook-form';
import { FiTrash2 } from 'react-icons/fi';

export const ArgumentsInput = ({
  name,
  types,
  disabled,
}: {
  name: string;
  types: string[];
  disabled?: boolean;
}) => {
  const { control } = useFormContext();

  const { fields, append, remove } = useFieldArray({
    control,
    name,
  });

  return (
    <div>
      <label className={clsx('block pt-1 text-gray-600 mb-1')}>
        <span className={clsx('flex items-center')}>
          <span className={clsx('font-semibold')}>Arguments</span>
        </span>
      </label>
      {fields.length ? (
        <CardedTable
          columns={['Argument name', 'Type', 'Nullable', 'Actions']}
          data={fields.map((field, index) => [
            <InputField
              key={`${field.id}-name`}
              dataTestId={`${name}[${index}].name`}
              name={`${name}[${index}].name`}
              fieldProps={{
                placeholder: 'Field Name',
                disabled,
              }}
            />,
            <SelectField
              key={`${field.id}-type`}
              name={`${name}[${index}].type`}
              options={types.map((t) => ({ label: t, value: t }))}
              disabled={disabled}
            />,
            <SwitchField
              key={`${field.id}-nullable`}
              name={`${name}[${index}].nullable`}
              disabled={disabled}
            />,
            <Button
              key={`${field.id}-action`}
              leftIcon={FiTrash2}
              onClick={() => remove(index)}
              mode="destructive"
              disabled={disabled}
            />,
          ])}
        />
      ) : null}
      <Button
        className="mb-2"
        mode="default"
        onClick={() => {
          append({ name: '', type: 'text', nullable: true });
        }}
        disabled={disabled}
      >
        Add new argument
      </Button>
    </div>
  );
};
