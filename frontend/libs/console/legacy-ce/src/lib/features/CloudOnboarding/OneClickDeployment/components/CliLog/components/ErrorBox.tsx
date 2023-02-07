import { Button } from '@/new-components/Button';
import React from 'react';
import {
  FaExclamationCircle,
  FaExternalLinkAlt,
  FaSyncAlt,
} from 'react-icons/fa';
import { capitalize } from '@/components/Common/utils/jsUtils';
import { UserFacingStep, FallbackApp } from '../../../types';
import { getErrorText } from '../utils';
import { LinkButton } from './LinkButton';
import { transformFallbackAppToLinkButtonProps } from '../fallbackAppUtil';

type Props = {
  step: UserFacingStep;
  error: Record<string, any>;
  retryAction: VoidFunction;
  fallbackApps: FallbackApp[];
};

export function ErrorBox(props: Props) {
  const { step, error, retryAction, fallbackApps } = props;

  const [isRetrying, setIsRetrying] = React.useState(false);
  React.useEffect(() => {
    let timeout: NodeJS.Timeout;
    if (isRetrying) {
      timeout = setTimeout(() => {
        setIsRetrying(false);
      }, 5000);
    }
    return () => {
      clearTimeout(timeout);
    };
  }, [isRetrying]);

  const onRetryClick = () => {
    if (!isRetrying && retryAction) {
      setIsRetrying(true);
      retryAction();
    }
  };

  const getErrorMessage = () => {
    if (error?.error?.message) {
      return capitalize(error.error.message);
    }
    return JSON.stringify(error);
  };

  return (
    <div className="font-sans bg-red-500/20 p-md border-l-red-500 border-l">
      <p className="font-bold text-white flex items-center">
        <FaExclamationCircle className="mr-xs" />
        {getErrorText(step)}
      </p>
      <p className="mb-sm text-white">{getErrorMessage()}</p>

      <div className="flex items-center">
        <Button
          mode="default"
          id="one-click-deployment-error-retry"
          data-testid="one-click-deployment-error-retry"
          isLoading={isRetrying}
          loadingText="Retrying"
          disabled={isRetrying}
          className="mr-sm bg-none bg-red-600 border-red-800 disabled:text-slate-50"
          icon={<FaSyncAlt className="text-white" />}
          iconPosition="start"
          onClick={onRetryClick}
        >
          <span className="text-white font-semibold text-md">Retry</span>
        </Button>
        <LinkButton
          url="https://hasura.io/docs/latest/hasura-cloud/one-click-deploy/index/#troubleshooting"
          buttonText="Troubleshooting Docs"
          icon={<FaExternalLinkAlt className="text-white" />}
          iconPosition="end"
        />
      </div>
      {fallbackApps.length ? (
        <>
          <div className="text-white mb-xs mt-md">
            Having trouble loading your project? Try one of our pre-made sample
            projects below:
          </div>
          <div className="flex items-center">
            {fallbackApps
              .map(transformFallbackAppToLinkButtonProps)
              .map(app => (
                <LinkButton {...app} />
              ))}
          </div>
        </>
      ) : null}
    </div>
  );
}
