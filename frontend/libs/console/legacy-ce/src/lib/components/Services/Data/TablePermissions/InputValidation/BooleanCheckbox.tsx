import { Controller, FieldError, useFormContext } from 'react-hook-form';
import get from 'lodash/get';
import {
  Checkbox,
  FieldWrapper,
  FieldWrapperPassThroughProps,
} from '../../../../../new-components/Form';

type BooleanCheckboxProps = FieldWrapperPassThroughProps & {
  name: string;
  disabled?: boolean;
  text: string;
};

export const BooleanCheckbox = ({
  name,
  disabled,
  text,
  ...wrapperProps
}: BooleanCheckboxProps) => {
  const {
    control,
    formState: { errors },
  } = useFormContext();
  const maybeError = get(errors, name) as FieldError | undefined;
  return (
    <FieldWrapper id={name} {...wrapperProps} error={maybeError}>
      <Controller
        control={control}
        name={name}
        render={({ field: { onChange, value } }) => (
          <Checkbox
            data-testid="checkbox"
            checked={value}
            onCheckedChange={onChange}
            disabled={disabled}
          >
            {text}
          </Checkbox>
        )}
      />
    </FieldWrapper>
  );
};
