import React from 'react';
import { MdRefresh } from 'react-icons/md';
import { FaExclamationCircle, FaPlusCircle, FaSpinner } from 'react-icons/fa';
import { useFormContext } from 'react-hook-form';
import { ErrorComponentTemplate } from '@/new-components/Form';
import { Button } from '@/new-components/Button';
import { NeonButtonIcons, NeonButtonProps } from '../../types';
import { NeonIcon } from './NeonIcon';
import { RequiredEnvVar } from '../../../../types';

export type Props = {
  dbEnvVar: RequiredEnvVar;
  neonButtonProps: NeonButtonProps;
};

const neonButtonIconMap: Record<NeonButtonIcons, JSX.Element> = {
  refresh: <MdRefresh className="text-black" />,
  create: <FaPlusCircle className="text-black" />,
  loading: <FaSpinner className="text-black animate-spin" />,
};

export function NeonButton(props: Props) {
  const { dbEnvVar, neonButtonProps } = props;
  const { formState } = useFormContext();

  let errorMessage: string | undefined;

  if (formState.errors?.[dbEnvVar.Name]) {
    errorMessage = formState.errors?.[dbEnvVar.Name].message;
  }
  if (neonButtonProps.status.status === 'error') {
    errorMessage = neonButtonProps.status.errorDescription;
  }

  return (
    <>
      <div className="mb-[0.45] flex items-center">
        <Button
          onClick={neonButtonProps.onClickConnect}
          icon={
            neonButtonProps.icon
              ? neonButtonIconMap[neonButtonProps.icon]
              : undefined
          }
          size="md"
          disabled={neonButtonProps.status.status === 'loading'}
        >
          <span className="text-lg tracking-tight text-black">
            {neonButtonProps.buttonText}
          </span>
        </Button>
        <div className="ml-sm text-gray-600">
          Powered by
          <span className="ml-xs">
            <NeonIcon />
          </span>
        </div>
      </div>

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
      ) : (
        // this acts as an empty placeholder for error, to prevent the form fields
        // from shifting down when an error message is shown
        <ErrorComponentTemplate label={<>&nbsp;</>} />
      )}
    </>
  );
}
