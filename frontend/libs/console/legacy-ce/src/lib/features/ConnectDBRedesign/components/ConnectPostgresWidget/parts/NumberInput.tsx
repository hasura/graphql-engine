import {
  FieldWrapper,
  FieldWrapperPassThroughProps,
} from '../../../../../new-components/Form';
import clsx from 'clsx';
import get from 'lodash/get';
import React, { useState } from 'react';
import { useFormContext } from 'react-hook-form';

type NumberInputProps = FieldWrapperPassThroughProps & {
  name: string;
  placeholder: string;
};

export const NumberInputField = ({
  name,
  placeholder,
  ...wrapperProps
}: NumberInputProps) => {
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
    <FieldWrapper id={name} error={maybeError} {...wrapperProps}>
      <div className={clsx('relative flex')}>
        <input
          id={name}
          placeholder={placeholder}
          type="number"
          aria-invalid={maybeError ? 'true' : 'false'}
          aria-label={name}
          onChange={e => {
            setLocalValue(e.target.value);

            // will fallback to undefined if input is empty or NaN
            setValue(name, parseInt(e.target.value, 10) || undefined);
          }}
          data-test={name}
          className={clsx(
            'block w-full h-input shadow-sm rounded border border-gray-300 hover:border-gray-400 focus-visible:outline-0 focus-visible:ring-2 focus-visible:ring-yellow-200 focus-visible:border-yellow-400 placeholder-gray-500'
          )}
          data-testid={name}
          value={localValue}
          onWheelCapture={e => e.currentTarget.blur()}
        />
      </div>
    </FieldWrapper>
  );
};
