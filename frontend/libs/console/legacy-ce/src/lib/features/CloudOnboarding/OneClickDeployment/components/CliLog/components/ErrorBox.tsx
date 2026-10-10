import { Button } from '@hasura/shared/ui';
import { Analytics } from '@hasura/shared/analytics';
import { Flex } from '@radix-ui/themes';
import React from 'react';
import {
  FaExclamationCircle,
  FaExternalLinkAlt,
  FaSyncAlt,
} from 'react-icons/fa';
import { UserFacingStep, FallbackApp } from '../../../types';
import { getErrorText, getProjectEnvVarPageLink } from '../utils';
import { LinkButton } from './LinkButton';
import { transformFallbackAppToLinkButtonProps } from '../fallbackAppUtil';
import { capitalize } from 'inflection';

type Props = {
  step: UserFacingStep;
  error: Record<string, any>;
  logId: string;
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
    let errorMsg: string | React.ReactElement<any> = '';

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
    <div className="font-sans bg-red-500/20 p-4 border-l-red-500 border-l">
      <p className="font-bold text-white flex items-center">
        <FaExclamationCircle className="mr-1" />
        {getErrorText(step)}
      </p>
      <p className="mt-2 mb-4 text-white">{getErrorMessage()}</p>

      <Flex align="center" gap="2">
        <Analytics
          name="one-click-deployment-error-retry"
          passHtmlAttributesToChildren
        >
          <Button
            mode="destructive"
            id="one-click-deployment-error-retry"
            data-testid="one-click-deployment-error-retry"
            loading={isRetrying}
            loadingText="Retrying"
            disabled={isRetrying}
            leftIcon={FaSyncAlt}
            onClick={onRetryClick}
          >
            Retry
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
            icon={FaExternalLinkAlt}
            iconPosition="end"
          />
        </Analytics>
      </Flex>
      {fallbackApps.length ? (
        <>
          <div className="mb-2 mt-4">
            <span className="text-white">
              Having trouble loading your project? Try one of our pre-made
              sample projects below:
            </span>
          </div>
          <Flex align="center" gap="2">
            {fallbackApps
              .map(transformFallbackAppToLinkButtonProps)
              .map((app) => (
                <Analytics
                  key={app.buttonText}
                  name={`one-click-deployment-fallback-app-${app.buttonText}`}
                  passHtmlAttributesToChildren
                >
                  <LinkButton
                    id={`one-click-deployment-fallback-app-${app.buttonText}`}
                    {...app}
                  />
                </Analytics>
              ))}
          </Flex>
        </>
      ) : null}
    </div>
  );
}
