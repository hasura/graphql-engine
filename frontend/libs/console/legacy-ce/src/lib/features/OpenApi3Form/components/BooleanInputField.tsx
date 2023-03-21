import { FieldWrapper } from '../../../new-components/Form';
import { Switch } from '../../../new-components/Switch';
import { OpenApiSchema } from '@hasura/dc-api-types';
import React, { useEffect } from 'react';
import { Controller, useFormContext } from 'react-hook-form';
import get from 'lodash/get';
import { getInputAttributes } from '../utils';

export const isBooleanInputField = (configSchema: OpenApiSchema) =>
  configSchema.type === 'boolean';

export const BooleanInputField = ({
  name,
  configSchema,
}: {
  name: string;
  configSchema: OpenApiSchema;
}) => {
  const { tooltip, label } = getInputAttributes(name, configSchema);
  const {
    watch,
    setValue,
    formState: { errors },
  } = useFormContext<Record<string, boolean | undefined>>();
  const maybeError = get(errors, name);

  const formValue = watch(name);

  useEffect(() => {
    setValue(name, !!formValue);
  }, [formValue, name, setValue]);

  return (
    <FieldWrapper
      id={name}
      error={maybeError}
      label={label}
      size="full"
      tooltip={tooltip}
    >
      <div className="max-w-xl flex justify-between my-4">
        <Controller
          name={name}
          render={({ field: { onChange, value } }) => (
            <Switch
              checked={value}
              onCheckedChange={onChange}
              data-testid={name}
            />
          )}
        />
      </div>
    </FieldWrapper>
  );
};
