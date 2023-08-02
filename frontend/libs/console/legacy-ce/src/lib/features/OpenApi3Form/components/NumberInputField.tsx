import { FieldWrapper } from '../../../new-components/Form';
import { OpenApiSchema } from '@hasura/dc-api-types';
import clsx from 'clsx';
import get from 'lodash/get';
import React, { useState } from 'react';
import { useFormContext } from 'react-hook-form';
import {
  getInputAttributes,
  getReferenceObject,
  isReferenceObject,
} from '../utils';

export const isNumberInputField = (
  configSchema: OpenApiSchema,
  otherSchemas: Record<string, OpenApiSchema>
): boolean => {
  const type = configSchema.type;

  /**
   * if its of type 'string'
   */
  if (type === 'integer' || type === 'number') return true;

  /**
   * if its of type 'array' then check the type of its items
   */
  if (type === 'array') {
    const items = configSchema.items;

    if (!items) return false;

    const itemsSchema = isReferenceObject(items)
      ? getReferenceObject(items.$ref, otherSchemas)
      : items;

    return isNumberInputField(itemsSchema, otherSchemas);
  }

  return false;
};

export const NumberInputField = ({
  name,
  configSchema,
}: {
  name: string;
  configSchema: OpenApiSchema;
}) => {
  const { tooltip, label } = getInputAttributes(name, configSchema);

  const {
    setValue,
    watch,
    formState: { errors },
  } = useFormContext<Record<string, number | undefined>>();
  const parentFormValue = watch(name);
  const [localValue, setLocalValue] = useState<string>(
    (parentFormValue ?? '').toString()
  );

  const maybeError = get(errors, name);

  return (
    <FieldWrapper id={name} error={maybeError} tooltip={tooltip} label={label}>
      <div className={clsx('relative flex max-w-xl')}>
        <input
          id={name}
          type="number"
          aria-invalid={maybeError ? 'true' : 'false'}
          aria-label={name}
          onChange={e => {
            setLocalValue(e.target.value);
            setValue(name, parseInt(e.target.value, 10));
          }}
          data-test={name}
          className={clsx(
            'block w-full h-input shadow-sm rounded border border-gray-300 hover:border-gray-400 focus-visible:outline-0 focus-visible:ring-2 focus-visible:ring-yellow-200 focus-visible:border-yellow-400 placeholder-gray-500'
          )}
          data-testid={name}
          value={localValue}
        />
      </div>
    </FieldWrapper>
  );
};
