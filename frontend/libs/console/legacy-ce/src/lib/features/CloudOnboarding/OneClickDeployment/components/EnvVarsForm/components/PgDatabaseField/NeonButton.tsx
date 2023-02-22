import React from 'react';
import { MdRefresh } from 'react-icons/md';
import { Analytics } from '../../../../../../Analytics';
import { FaExclamationCircle, FaPlusCircle, FaSpinner } from 'react-icons/fa';
import { useFormContext } from 'react-hook-form';
import { ErrorComponentTemplate } from '../../../../../../../new-components/Form';
import { Button } from '../../../../../../../new-components/Button';
import { NeonButtonIcons, NeonButtonProps } from '../../types';
import { RequiredEnvVar } from '../../../../types';

export type Props = {
  dbEnvVar: RequiredEnvVar;
  neonButtonProps: NeonButtonProps;
};

const neonButtonIconMap: Record<NeonButtonIcons, JSX.Element> = {
  refresh: <MdRefresh className="text-slate-900" />,
  create: <FaPlusCircle className="text-slate-900" />,
  loading: <FaSpinner className="text-slate-900 animate-spin" />,
};

export function NeonButton(props: Props) {
  const { dbEnvVar, neonButtonProps } = props;
  const { formState } = useFormContext();

  let errorMessage: string | undefined | React.ReactNode;

  if (formState.errors?.[dbEnvVar.Name]) {
    errorMessage = formState.errors?.[dbEnvVar.Name].message;
  }
  if (neonButtonProps.status.status === 'error') {
    errorMessage = neonButtonProps.status.errorDescription;
  }

  return (
    <>
      <div className="flex items-center">
        <Analytics
          name="one-click-deployment-neon-button"
          passHtmlAttributesToChildren
        >
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
            <span className="text-lg font-bold text-slate-900">
              {neonButtonProps.buttonText}
            </span>
          </Button>
        </Analytics>
      </div>

      {errorMessage ? (
        <ErrorComponentTemplate
          label={
            <>
              <FaExclamationCircle className="fill-current h-4 w-4 mr-xs shrink-0" />
              {errorMessage}
            </>
          }
          ariaLabel={errorMessage ? 'Neon Database creation failed' : ''}
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
