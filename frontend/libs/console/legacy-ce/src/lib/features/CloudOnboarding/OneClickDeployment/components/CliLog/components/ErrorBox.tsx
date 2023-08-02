import { Button } from '../../../../../../new-components/Button';
import { Analytics } from '../../../../../Analytics';
import React from 'react';
import {
  FaExclamationCircle,
  FaExternalLinkAlt,
  FaSyncAlt,
} from 'react-icons/fa';
import { capitalize } from '../../../../../../components/Common/utils/jsUtils';
import { UserFacingStep, FallbackApp } from '../../../types';
import { getErrorText, getProjectEnvVarPageLink } from '../utils';
import { LinkButton } from './LinkButton';
import { transformFallbackAppToLinkButtonProps } from '../fallbackAppUtil';

type Props = {
  step: UserFacingStep;
  error: Record<string, any>;
  logId: number;
  retryAction: VoidFunction;
  fallbackApps: FallbackApp[];
};

export function ErrorBox(props: Props) {
  const { step, error, retryAction, fallbackApps, logId } = props;

  const [isRetrying, setIsRetrying] = React.useState(false);

  // mark retry as complete if logId is different
  React.useEffect(() => {
    setIsRetrying(false);
  }, [logId]);

  const onRetryClick = () => {
    if (!isRetrying && retryAction) {
      setIsRetrying(true);
      retryAction();
    }
  };

  const getErrorMessage = () => {
    let errorMsg: string | React.ReactElement = '';

    if (error?.error?.message) {
      errorMsg = capitalize(error.error.message);
    } else {
      errorMsg = JSON.stringify(error);
    }

    // add link to project env vars page in case of database connection error
    if (errorMsg.includes('Database connection error')) {
      errorMsg = (
        <>
          <div className="whitespace-pre-line">{errorMsg}</div>
          <br />
          <div>
            You can update the project environment variables{' '}
            <a
              href={getProjectEnvVarPageLink()}
              target="_blank"
              rel="noreferrer noopener"
              className="text-zinc-400 hover:text-zinc-500"
            >
              here
            </a>
            .
          </div>
        </>
      );
    }

    return errorMsg;
  };

  return (
    <div className="font-sans bg-red-500/20 p-md border-l-red-500 border-l">
      <p className="font-bold text-white flex items-center">
        <FaExclamationCircle className="mr-xs" />
        {getErrorText(step)}
      </p>
      <p className="mb-sm text-white">{getErrorMessage()}</p>

      <div className="flex items-center">
        <Analytics
          name="one-click-deployment-error-retry"
          passHtmlAttributesToChildren
        >
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
        </Analytics>
        <Analytics
          name="one-click-deployment-error-trouble-shooting-button"
          passHtmlAttributesToChildren
        >
          <LinkButton
            id="one-click-deployment-error-trouble-shooting-button"
            url="https://hasura.io/docs/latest/hasura-cloud/one-click-deploy/index/#troubleshooting"
            buttonText="Troubleshooting Docs"
            icon={<FaExternalLinkAlt className="text-white" />}
            iconPosition="end"
          />
        </Analytics>
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
                <Analytics
                  name={`one-click-deployment-fallback-app-${app.buttonText}`}
                  passHtmlAttributesToChildren
                >
                  <LinkButton
                    id={`one-click-deployment-fallback-app-${app.buttonText}`}
                    {...app}
                  />
                </Analytics>
              ))}
          </div>
        </>
      ) : null}
    </div>
  );
}
