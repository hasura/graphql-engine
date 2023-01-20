import React from 'react';
import { Dialog } from '@/new-components/Dialog';
import { MdRefresh } from 'react-icons/md';
import { EnvVarsFormState } from '../../../types';

type Props = { formState: EnvVarsFormState };

export function CustomFooter(props: Props) {
  const { formState: state } = props;

  const buttonText = {
    default: 'Update Environment Variables',
    loading: 'Updating...',
    success: 'Updated! Please wait...',
    error: 'Retry Updating Environment Variables',
    hidden: '',
  };

  return (
    <Dialog.Footer
      callToAction={buttonText[state]}
      callToActionIcon={
        state === 'error' ? <MdRefresh className="text-black" /> : undefined
      }
      disabled={state === 'loading' || state === 'success'}
      isLoading={state === 'loading'}
      callToActionLoadingText={buttonText.loading}
      onClose={() => {}}
    />
  );
}
