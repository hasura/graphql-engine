import { FaExclamationTriangle } from 'react-icons/fa';
import {
  Button,
  Tooltip,
  LearnMoreLink,
  IconButton,
  Text,
} from '@hasura/shared/ui';
import { useLocalStorage } from '@hasura/shared/hooks';
import { Analytics } from '@hasura/shared/analytics';
import { hasuraToast } from '@hasura/shared/ui';
import { isCachingEnabled, getErrorMessage } from '@hasura/shared/utils';
import { useGraphiQL } from '@graphiql/react';
import { toggleCacheDirective } from './utils';
import { Flex } from '@radix-ui/themes';
import { useAppContext } from '@hasura/shared/context';

export const RESPONSE_TIME_CACHE_WARNING = 1000;
const DISMISS_TIME = 24 * 60 * 60 * 1000;
interface ResponseTimeWarningProps {}

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

export const ResponseTimeWarning: React.FC<ResponseTimeWarningProps> = () => {
  const { envVars } = useAppContext();
  // GraphiQL v5: parsed `operations` live on the store (not on the Monaco
  // `queryEditor`); the editor instance is still read from the store for
  // `setValue`.
  const operations = useGraphiQL((state) => state.operations);
  const queryEditor = useGraphiQL((state) => state.queryEditor);
  const [dismissStatus, setDismissStatus] = useLocalStorage<DismissStatus>(
    'api-response-time-warning-dismiss',
    {
      state: 'not-dismissed',
    },
  );

  const isDismissed = isWarningDismissed(dismissStatus);

  const addCacheDirective = () => {
    if (!operations?.length || !queryEditor) {
      return;
    }

    try {
      const cacheToggledOperationString = toggleCacheDirective(
        operations,
        true,
      );
      if (!cacheToggledOperationString) {
        return;
      }

      queryEditor.setValue(cacheToggledOperationString);
    } catch (err) {
      hasuraToast({
        type: 'error',
        title: 'Failed to add @cached directive',
        message: getErrorMessage(err),
      });
    }
  };

  if (!isCachingEnabled(envVars)) {
    return null;
  }

  return (
    <div>
      <Tooltip
        defaultOpen={!isDismissed}
        content={
          <div>
            <Text>
              This query took a long time to execute. Consider adding caching
              directives to your query to improve performance.
            </Text>{' '}
            <LearnMoreLink
              href="https://hasura.io/docs/latest/caching/quickstart/"
              text="(Learn More)"
              weight="bold"
            />
            <Flex justify="end" className="mt-2" gap="2">
              <Analytics
                name="api-explorer-response-time-warning-add-cache-directive"
                passHtmlAttributesToChildren
              >
                <Button size="1" mode="primary" onClick={addCacheDirective}>
                  Add caching directives
                </Button>
              </Analytics>
              <Analytics
                name={dismissNextStateConfig[dismissStatus.state].tracking}
                passHtmlAttributesToChildren
              >
                <Button
                  size="1"
                  mode="default"
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
            </Flex>
          </div>
        }
        side="top"
      >
        <IconButton
          color="yellow"
          variant="ghost"
          radius="full"
          icon={FaExclamationTriangle}
        />
      </Tooltip>
    </div>
  );
};
