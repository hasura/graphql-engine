import React from 'react';
import { Dialog } from '@/new-components/Dialog';
import { MdRefresh } from 'react-icons/md';
import { EnvVarsFormState } from '../../../types';

type Props = { formState: EnvVarsFormState };

export function CustomFooter(props: Props) {
  const { formState: state } = props;

  const buttonText = {
    default: 'Set Environment Variables',
    loading: 'Setting...',
    error: 'Retry Setting Environment Variables',
    hidden: '',
  };

  return (
    <Dialog.Footer
      callToAction={buttonText[state]}
      callToActionIcon={
        state === 'error' ? <MdRefresh className="text-black" /> : undefined
      }
      disabled={state === 'loading'}
      isLoading={state === 'loading'}
      callToActionLoadingText={buttonText.loading}
      onClose={() => {}}
    />
  );
}
