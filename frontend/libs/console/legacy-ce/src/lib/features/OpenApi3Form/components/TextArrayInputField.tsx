import { OpenApiSchema } from '@hasura/dc-api-types';
import React, { useState } from 'react';
import clsx from 'clsx';
import { FieldWrapper } from '../../../new-components/Form';
import get from 'lodash/get';
import { FieldError, useFormContext } from 'react-hook-form';
import {
  getInputAttributes,
  getReferenceObject,
  isReferenceObject,
} from '../utils';
import { isTextInputField } from './TextInputField';

export const isTextArrayInputField = (
  configSchema: OpenApiSchema,
  otherSchemas: Record<string, OpenApiSchema>
): boolean => {
  const type = configSchema.type;
  /**
   * if its of type 'array' then check the type of its items
   */
  if (type === 'array') {
    const items = configSchema.items;

    if (!items) return false;

    const itemsSchema = isReferenceObject(items)
      ? getReferenceObject(items.$ref, otherSchemas)
      : items;

    return isTextInputField(itemsSchema);
  }
  return false;
};

export const TextArrayInputField = ({
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
  } = useFormContext<Record<string, string[] | undefined>>();
  const parentFormValue = watch(name);
  const [localValue, setLocalValue] = useState<string>(
    parentFormValue ? parentFormValue.join(',') : ''
  );

  const maybeError = get(errors, name) as unknown as FieldError | undefined;

  return (
    <FieldWrapper id={name} error={maybeError} label={label} tooltip={tooltip}>
      <div className={clsx('relative flex max-w-xl')}>
        <input
          id={name}
          type="text"
          aria-invalid={maybeError ? 'true' : 'false'}
          aria-label={name}
          onChange={e => {
            const val = e.target.value;
            setLocalValue(e.target.value);
            setValue(name, val ? val.split(',') : []);
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
