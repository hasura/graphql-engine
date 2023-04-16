import React from 'react';
import { FaExclamationCircle } from 'react-icons/fa';

import { ErrorComponentTemplate } from '../../../../new-components/Form';

type Props = {
  message: React.ReactElement;
};

export function ErrorMessage(props: Props) {
  const { message } = props;
  return (
    <ErrorComponentTemplate
      label={
        <>
          <FaExclamationCircle className="fill-current h-4 w-4 mr-xs shrink-0" />
          {message}
        </>
      }
      role="alert"
    />
  );
}
