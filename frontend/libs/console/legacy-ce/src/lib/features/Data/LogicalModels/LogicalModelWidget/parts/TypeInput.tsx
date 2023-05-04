import { Controller } from 'react-hook-form';
import {
  FieldWrapper,
  FieldWrapperPassThroughProps,
  MultiSelectItem,
} from '../../../../../new-components/Form';

type TypeInputProps = FieldWrapperPassThroughProps & {
  name: string;
  options: MultiSelectItem[];
  disabled?: boolean;
};

export const TypeInput = ({
  name,
  options,
  disabled,
  ...wrapperProps
}: TypeInputProps) => {
  return (
    <Controller
      name={name}
      render={({ field: { value }, fieldState }) => (
        <FieldWrapper id={name} {...wrapperProps} error={fieldState.error}>
          <select
            data-testid={name}
            className={
              'block w-full h-input shadow-sm rounded border border-gray-300 hover:border-gray-400 focus-visible:outline-0 focus-visible:ring-2 focus-visible:ring-yellow-200 focus-visible:border-yellow-400 text-gray-500'
            }
            value={value}
            disabled={disabled}
          >
            {options.map(({ label, value }) => (
              <option key={`${name}-${value}`} value={value}>
                {label}
              </option>
            ))}
          </select>
        </FieldWrapper>
      )}
    />
  );
};
