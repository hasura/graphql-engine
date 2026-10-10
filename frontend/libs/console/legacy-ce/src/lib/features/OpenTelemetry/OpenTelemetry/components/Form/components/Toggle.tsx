import type { ZodType } from 'zod';
import type { FieldError, FieldPath } from 'react-hook-form';
import { Switch, FieldWrapper } from '@hasura/shared/ui';
import type { FieldWrapperPassThroughProps } from '@hasura/shared/ui';
import { z } from 'zod';
import get from 'lodash/get';
import { useFormContext, useWatch } from 'react-hook-form';

type TFormValues = Record<string, unknown>;

export type Schema = ZodType<TFormValues, TFormValues>;

export type ToggleProps<T extends z.infer<Schema>> =
  FieldWrapperPassThroughProps & {
    /**
     * The input field name
     */
    name: FieldPath<T>;
    /**
     * The input field classes
     */
    className?: string;
    /**
     * Flag to indicate if the field is disabled
     */
    disabled?: boolean;
    /**
     * The written status can clarify to the user what the on/off statuses mean
     */
    writtenStatus?: { true: string; false: string };
  };

// TODO: Promote it to a shared component
export const Toggle = <T extends z.infer<Schema>>({
  name,
  disabled,
  className,
  writtenStatus,
  renderDescriptionLineBreaks = false,
  ...wrapperProps
}: ToggleProps<T>) => {
  const {
    register,
    formState: { errors },
  } = useFormContext<T>();

  const { onChange, ...regReturn } = register(name);
  const value = !!useWatch<Record<string, boolean>>({ name });
  const maybeError = get(errors, name) as FieldError | undefined;

  const onCheckedChange = (newValue: boolean) => {
    const fakeEvent = {
      target: { value: newValue, checked: newValue, name },
    };

    // `return` is here only to respect the default React Hook Form behavior
    return onChange(fakeEvent);
  };

  const index = value.toString() === 'true' ? 'true' : 'false';
  const status = writtenStatus?.[index];

  return (
    <FieldWrapper
      id={name}
      {...wrapperProps}
      size="full"
      error={maybeError}
      renderDescriptionLineBreaks={renderDescriptionLineBreaks}
    >
      <div className={`flex ${className ?? ''}`}>
        <Switch
          id={name}
          aria-label={wrapperProps.label}
          aria-invalid={maybeError ? 'true' : 'false'}
          {...regReturn}
          value={value}
          data-testid={name}
          disabled={disabled}
          defaultChecked={value}
          onChange={onCheckedChange}
        />

        {/* ml-2 is used in place of the usual ml-1 because otherwise the green border of the focus
        effect sticks to the string */}
        {!!status && <span className="ml-2">{status}</span>}
      </div>
    </FieldWrapper>
  );
};
