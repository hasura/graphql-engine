import { Button } from '@/new-components/Button';
import React from 'react';
import {
  FaExclamationCircle,
  FaExternalLinkAlt,
  FaSyncAlt,
} from 'react-icons/fa';
import { UserFacingStep } from '../../../types';
import { getErrorText } from '../utils';

type Props = {
  step: UserFacingStep;
  error: Record<string, string>;
  retryAction: VoidFunction;
};

export function ErrorBox(props: Props) {
  const { step, error, retryAction } = props;

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

  return (
    <div className="bg-red-500/20 p-md border-l-red-500 border-l">
      <p className="font-bold text-white flex items-center">
        <FaExclamationCircle className="mr-xs" />
        {getErrorText(step)}
      </p>
      <p className="mb-sm text-white">Error: {JSON.stringify(error)}</p>
      <div className="flex items-center">
        <Button
          mode="default"
          id="one-click-deployment-error-retry"
          data-testid="one-click-deployment-error-retry"
          isLoading={isRetrying}
          loadingText="Retrying"
          disabled={isRetrying}
          className="mr-sm bg-none bg-red-500 border-none disabled:text-white"
          icon={<FaSyncAlt className="text-white" />}
          iconPosition="start"
          onClick={onRetryClick}
        >
          <span className="text-white font-semibold text-md">Retry</span>
        </Button>
        <a
          href="https://hasura.io/docs"
          id="one-click-deployment-error-troubleshooting-docs"
          data-testid="one-click-deployment-error-troubleshooting-docs"
          target="_blank"
          rel="noopener noreferrer"
        >
          <Button
            mode="default"
            className="bg-none bg-transparent border-red-500"
            icon={<FaExternalLinkAlt className="text-white" />}
            iconPosition="end"
          >
            <span className="text-white font-semibold text-md">
              Troubleshooting Docs
            </span>
          </Button>
        </a>
      </div>
    </div>
  );
}
