import React from 'react';
import { Dialog } from '../../../../../../new-components/Dialog';
import { Analytics } from '../../../../../Analytics';
import { MdRefresh } from 'react-icons/md';
import { EnvVarsFormState } from '../../../types';

type Props = { formState: EnvVarsFormState };

export function CustomFooter(props: Props) {
  const { formState: state } = props;

  const buttonText = {
    default: 'Set Environment Variables',
    loading: 'Setting Environment Variables...',
    error: 'Retry Setting Environment Variables',
    hidden: '',
  };

  return (
    <Analytics
      name="one-click-deployment-env-var-form-submit"
      passHtmlAttributesToChildren
    >
      <Dialog.Footer
        id="one-click-deployment-env-var-form-submit"
        callToAction={buttonText[state]}
        callToActionIcon={
          state === 'error' ? <MdRefresh className="text-black" /> : undefined
        }
        disabled={state === 'loading'}
        isLoading={state === 'loading'}
        callToActionLoadingText={buttonText.loading}
        onClose={() => {}}
      />
    </Analytics>
  );
}
