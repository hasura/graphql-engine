import {
  FieldWrapper,
  FieldWrapperPassThroughProps,
} from '../../../../../new-components/Form';
import { Switch } from '../../../../../new-components/Switch';
import get from 'lodash/get';
import { Controller, FieldError, useFormContext } from 'react-hook-form';

type BooleanInputProps = FieldWrapperPassThroughProps & {
  name: string;
};

export const BooleanInput = ({ name, ...wrapperProps }: BooleanInputProps) => {
  const {
    formState: { errors },
  } = useFormContext();

  const maybeError = get(errors, name) as FieldError | undefined;

  return (
    <Controller
      name={name}
      render={({ field: { onChange, value } }) => (
        <FieldWrapper id={name} {...wrapperProps} error={maybeError}>
          <Switch checked={value} onCheckedChange={onChange} />
        </FieldWrapper>
      )}
    />
  );
};
