import get from 'lodash/get';
import { FieldError, useFormContext, Controller } from 'react-hook-form';
import { FieldWrapper, FieldWrapperPassThroughProps } from '../FieldWrapper';
import {
  CheckboxGroup,
  CheckboxGroupProps,
  CheckboxItem,
} from '../../base/CheckboxGroup';

export type CheckboxesFieldProps = FieldWrapperPassThroughProps & {
  /**
   * The checkbox name
   */
  name: string;
  /**
   * The options to display with checkboxes
   */
  options: CheckboxItem[];
  /**
   * Flag to indicate if the field is disabled
   */
  disabled?: boolean;
  /**
   * Removing styling only necessary for the error placeholder
   */
  noErrorPlaceholder?: boolean;

  fieldProps?: Omit<
    CheckboxGroupProps,
    'disabled' | 'name' | 'disabled' | 'options' | 'orientation'
  >;
};

export function CheckboxesField({
  name,
  options = [],
  orientation = 'vertical',
  disabled = false,
  dataTest,
  noErrorPlaceholder,
  fieldProps,
  ...wrapperProps
}: CheckboxesFieldProps) {
  const {
    control,
    formState: { errors },
  } = useFormContext();

  const maybeError = get(errors, name) as FieldError | undefined;
  return (
    <FieldWrapper
      noErrorPlaceholder={noErrorPlaceholder}
      id={name}
      {...wrapperProps}
      error={maybeError}
    >
      <Controller
        name={name}
        control={control}
        render={({ field }) => (
          <CheckboxGroup
            orientation={orientation}
            options={options}
            loading={wrapperProps.loading}
            {...field}
          />
        )}
      />
    </FieldWrapper>
  );
}
