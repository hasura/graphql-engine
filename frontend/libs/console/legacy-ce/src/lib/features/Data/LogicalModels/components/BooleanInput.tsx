import {
  FieldWrapper,
  FieldWrapperPassThroughProps,
} from '../../../../new-components/Form';
import { Switch } from '../../../../new-components/Switch';
import { Controller } from 'react-hook-form';

type BooleanInputProps = FieldWrapperPassThroughProps & {
  name: string;
  disabled?: boolean;
};

export const BooleanInput = ({
  name,
  disabled,
  ...wrapperProps
}: BooleanInputProps) => {
  return (
    <Controller
      name={name}
      render={({ field: { onChange, value }, fieldState }) => (
        <FieldWrapper
          id={name}
          {...wrapperProps}
          className="items-center flex"
          doNotWrapChildren
          error={fieldState.error}
        >
          <Switch
            data-testid={wrapperProps.dataTestId || 'switch'}
            checked={value}
            onCheckedChange={onChange}
            disabled={disabled}
          />
        </FieldWrapper>
      )}
    />
  );
};
