import React, { useEffect } from 'react';
import { useFormContext } from 'react-hook-form';
import {
  Checkbox,
  ErrorComponentTemplate,
} from '../../../../../new-components/Form';
import { CheckedState } from '@radix-ui/react-checkbox';
import { FaExclamationCircle } from 'react-icons/fa';

type Props = {
  fieldName: string;
};

export const ConsentCheckbox = (props: Props) => {
  const { fieldName } = props;
  const { watch, setValue, formState } = useFormContext();
  const field = watch(fieldName);
  const [errorMessage, setErrorMessage] = React.useState('');

  useEffect(() => {
    if (field) {
      setErrorMessage('');
    } else if (formState?.errors?.[fieldName]) {
      setErrorMessage(formState?.errors?.[fieldName]?.message);
    } else {
      setErrorMessage('');
    }
  }, [formState?.errors?.[fieldName], field]);

  const onCheckedChange = (value: CheckedState) => {
    setValue(fieldName, value);
  };
  return (
    <>
      <Checkbox
        checked={field}
        name={fieldName}
        onCheckedChange={onCheckedChange}
      >
        By signing up for Hasura Enterprise Edition, you acknowledge that you
        agree to our{' '}
        <a
          href="https://hasura.io/legal/hasura-ee-trial-terms-of-service/"
          target="_blank"
          rel="noopener noreferrer"
        >
          Terms of Service
        </a>{' '}
        and{' '}
        <a
          href="https://hasura.io/legal/hasura-privacy-policy"
          target="_blank"
          rel="noopener noreferrer"
        >
          Privacy Policy
        </a>
      </Checkbox>
      {errorMessage ? (
        <ErrorComponentTemplate
          label={
            <>
              <FaExclamationCircle className="fill-current h-4 w-4 mr-xs shrink-0" />
              {errorMessage}
            </>
          }
          ariaLabel={errorMessage ?? ''}
          role="alert"
        />
      ) : null}
    </>
  );
};
