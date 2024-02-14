import { FaExclamationTriangle } from 'react-icons/fa';
import { Tooltip } from '../../../../new-components/Tooltip';
import { Button } from '../../../../new-components/Button';
import { useLocalStorage } from '../../../../hooks';
import { LearnMoreLink } from '../../../../new-components/LearnMoreLink';
import { Analytics } from '../../../../features/Analytics';
import { hasuraToast } from '../../../../new-components/Toasts';
import { isCachingEnabled } from '../../../../utils/proConsole';

export const RESPONSE_TIME_CACHE_WARNING = 1000;
const DISMISS_TIME = 24 * 60 * 60 * 1000;
interface ResponseTimeWarningProps {
  onAddCacheDirective: () => void;
}

type DismissStatus =
  | {
      state: 'permanently-dismissed';
      timestamp?: never;
    }
  | {
      state: 'temporarily-dismissed';
      timestamp: string;
    }
  | {
      state: 'not-dismissed';
      timestamp?: never;
    };

const dismissNextStateConfig: Record<
  DismissStatus['state'],
  { label: string; tracking: string; notification: string }
> = {
  'permanently-dismissed': {
    label: 'Remind me again',
    tracking: 'api-explorer-response-time-warning-show-again',
    notification: 'Response time warning will pop up again for slow queries',
  },
  'temporarily-dismissed': {
    label: 'Hide forever',
    tracking: 'api-explorer-response-time-warning-hide-forever',
    notification: 'Response time warning will not pop up for slow queries',
  },
  'not-dismissed': {
    label: 'Hide for today',
    tracking: 'api-explorer-response-time-warning-hide-today',
    notification:
      'Response time warning will not pop up for slow queries until tomorrow',
  },
};

const isWarningDismissed = (status: DismissStatus) => {
  if (status.state === 'permanently-dismissed') {
    return true;
  }
  if (status.state === 'not-dismissed') {
    return false;
  }
  const dismissTimestamp = status.timestamp;

  const dismissDate = new Date(dismissTimestamp);
  const now = new Date();
  return dismissDate.getTime() + DISMISS_TIME > now.getTime();
};

const toggleDismissStatus = (status: DismissStatus): DismissStatus => {
  if (status.state === 'permanently-dismissed') {
    return {
      state: 'not-dismissed',
    };
  }
  if (status.state === 'not-dismissed') {
    return {
      state: 'temporarily-dismissed',
      timestamp: new Date().toISOString(),
    };
  }
  return {
    state: 'permanently-dismissed',
  };
};

export const ResponseTimeWarning: React.VFC<
  ResponseTimeWarningProps
> = props => {
  const { onAddCacheDirective } = props;
  const [dismissStatus, setDismissStatus] = useLocalStorage<DismissStatus>(
    'api-response-time-warning-dismiss',
    {
      state: 'not-dismissed',
    }
  );

  const isDismissed = isWarningDismissed(dismissStatus);

  if (!isCachingEnabled(window.__env)) {
    return null;
  }

  return (
    <div className=" mr-md -ml-md">
      <Tooltip
        options={{}}
        theme="light"
        defaultOpen={!isDismissed}
        tooltipContentChildren={
          <div>
            This query took a long time to execute. Consider adding caching
            directives to your query to improve performance.{' '}
            <LearnMoreLink
              href="https://hasura.io/docs/latest/caching/quickstart/"
              text="(Learn More)"
              className="font-normal"
            />
            <div className="flex justify-end mt-4">
              <Analytics
                name="api-explorer-response-time-warning-add-cache-directive"
                passHtmlAttributesToChildren
              >
                <Button mode="primary" onClick={onAddCacheDirective}>
                  Add caching directives
                </Button>
              </Analytics>
              <Analytics
                name={dismissNextStateConfig[dismissStatus.state].tracking}
                passHtmlAttributesToChildren
              >
                <Button
                  className="ml-2"
                  onClick={() => {
                    setDismissStatus(toggleDismissStatus(dismissStatus));
                    hasuraToast({
                      type: 'info',
                      title: 'Response time warning',
                      message:
                        dismissNextStateConfig[dismissStatus.state]
                          .notification,
                    });
                  }}
                >
                  {dismissNextStateConfig[dismissStatus.state].label}
                </Button>
              </Analytics>
            </div>
          </div>
        }
        side="top"
      >
        <FaExclamationTriangle className="text-yellow-600 mb-1" />
      </Tooltip>
    </div>
  );
};
