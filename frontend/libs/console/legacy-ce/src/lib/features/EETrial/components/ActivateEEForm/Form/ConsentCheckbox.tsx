import React from 'react';
import { useFormContext } from 'react-hook-form';
import { Checkbox, ErrorMessage } from '@hasura/shared/ui';
import { CheckboxProps, Link } from '@radix-ui/themes';

type Props = {
  fieldName: string;
};

export const ConsentCheckbox = (props: Props) => {
  const { fieldName } = props;
  const { watch, setValue, formState } = useFormContext();
  const field = watch(fieldName);

  // The error message is fully derivable from the field value and the form
  // state, so we compute it during render instead of mirroring it into state
  // via an effect (which triggered cascading renders).
  const errorMessage =
    !field && formState?.errors?.[fieldName]?.message
      ? (formState.errors[fieldName].message as string)
      : '';

  const onCheckedChange = (value: CheckboxProps['checked']) => {
    setValue(fieldName, value);
  };
  return (
    <>
      <Checkbox value={field} name={fieldName} onChange={onCheckedChange}>
        <p>
          By signing up for Hasura Enterprise Edition, you acknowledge that you
          agree to our{' '}
          <Link
            href="https://hasura.io/legal/hasura-ee-trial-terms-of-service/"
            target="_blank"
            rel="noopener noreferrer"
          >
            Terms of Service
          </Link>{' '}
          and{' '}
          <Link
            href="https://hasura.io/legal/hasura-privacy-policy"
            target="_blank"
            rel="noopener noreferrer"
          >
            Privacy Policy
          </Link>
        </p>
      </Checkbox>
      {errorMessage ? <ErrorMessage error={errorMessage} /> : null}
    </>
  );
};
