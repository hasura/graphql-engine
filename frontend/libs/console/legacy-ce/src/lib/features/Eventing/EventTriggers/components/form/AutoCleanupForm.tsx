import { FaQuestionCircle } from 'react-icons/fa';
import { Flex, Heading } from '@radix-ui/themes';
import { Analytics } from '@hasura/shared/analytics';
import {
  Tooltip,
  Collapsible,
  DropdownButton,
  Switch,
  FieldWrapper,
  Input,
  IconTooltip,
  IconButton,
  Text,
  DropdownMenu,
} from '@hasura/shared/ui';

import { ETAutoCleanupWrapper } from '../../../../EETrial';
import { EventTriggerAutoCleanup } from '../../types';
import { safeParseInt } from '@hasura/shared/utils';

interface AutoCleanupFormProps {
  cleanupConfig?: EventTriggerAutoCleanup;
  onChange: (cleanupConfig: EventTriggerAutoCleanup) => void;
}

const crons = [
  { value: '* * * * *', label: 'Every Minute' },
  { value: '*/10 * * * *', label: 'Every 10 Minutes' },
  { value: '0 0 * * *', label: 'Every Midnight' },
  { value: '0 0 1 * *', label: 'Every Month Start' },
  { value: '0 12 * * 5', label: 'Every Friday Noon' },
];

export const AutoCleanupForm = (props: AutoCleanupFormProps) => {
  const { cleanupConfig, onChange } = props;
  const isCleanupConfigSet =
    cleanupConfig && Object.keys(cleanupConfig).length > 0;

  // disable other fields when cleanup is paused
  const isDisable = isCleanupConfigSet
    ? cleanupConfig?.paused
    : !cleanupConfig?.paused;

  return (
    <Analytics name="open-event-log-auto-cleanup" passHtmlAttributesToChildren>
      <Collapsible
        triggerChildren={
          <Flex align="center" gap="2">
            <Heading size="4">Auto-cleanup Event Logs</Heading>
            <Tooltip
              side="top"
              content={
                isCleanupConfigSet &&
                !(
                  cleanupConfig?.paused &&
                  Object.keys(cleanupConfig).length === 1
                )
                  ? 'Auto-cleanup has been configured. After clearing/resetting, save changes to remove the configuration.'
                  : 'Auto-cleanup is currently not configured'
              }
            >
              <Flex align="center" gap="2">
                <IconButton
                  type="button"
                  variant="ghost"
                  color={
                    isCleanupConfigSet &&
                    !(
                      cleanupConfig?.paused &&
                      Object.keys(cleanupConfig).length === 1
                    )
                      ? 'indigo'
                      : 'gray'
                  }
                  radius="full"
                >
                  <FaQuestionCircle />
                </IconButton>
                <Analytics
                  name="event-auto-cleanup-clear-reset-btn"
                  passHtmlAttributesToChildren
                >
                  <span
                    className="text-sky-500 ml-xs font-thin text-sm"
                    onClick={() => onChange({})}
                  >
                    {isCleanupConfigSet &&
                    !(
                      cleanupConfig?.paused &&
                      Object.keys(cleanupConfig).length === 1
                    )
                      ? 'Clear / Reset'
                      : ''}
                  </span>
                </Analytics>
              </Flex>
            </Tooltip>
          </Flex>
        }
        defaultOpen
      >
        <ETAutoCleanupWrapper>
          <Flex className="w-1/2" direction="column" gap="4">
            <Flex align="center" gap="2">
              <Switch
                value={cleanupConfig?.paused === false}
                onChange={(checked) => {
                  onChange({
                    ...cleanupConfig,
                    paused: checked,
                  });
                }}
              >
                Enable event log cleanup
              </Switch>
              <IconTooltip
                side="right"
                message={
                  isCleanupConfigSet
                    ? 'When not enabled, event log cleanup is paused. To completely remove event log cleanup configuration use Clear/Reset button'
                    : 'When not enabled, event log cleanup is paused'
                }
              />
            </Flex>
            <Flex align="center" gap="2">
              <Switch
                value={cleanupConfig?.clean_invocation_logs}
                disabled={isDisable}
                onChange={(checked) => {
                  onChange({
                    ...cleanupConfig,
                    clean_invocation_logs: checked,
                  });
                }}
              >
                Clean invocation logs with event logs
              </Switch>
              <IconTooltip
                side="right"
                message={
                  isDisable
                    ? 'Enable event log cleanup to configure'
                    : 'Enabling this will clear event invocation logs along with event logs'
                }
              />
            </Flex>

            <FieldWrapper
              noErrorPlaceholder
              label="Clear logs older than (hours)"
              tooltip={
                isDisable
                  ? `Enable event log cleanup to configure. Clear event logs older than (in hours)`
                  : `Clear event logs older than (in hours)`
              }
            >
              <Input
                type="number"
                disabled={isDisable}
                placeholder="168"
                required
                value={cleanupConfig?.clear_older_than?.toString() ?? ''}
                onChange={(ev) => {
                  onChange({
                    ...cleanupConfig,
                    clear_older_than: safeParseInt(ev.target.value, undefined),
                  });
                }}
              />
            </FieldWrapper>
            <FieldWrapper
              noErrorPlaceholder
              label="Cleanup Frequency"
              tooltip={
                isDisable
                  ? `Enable event log cleanup to configure. Cron expression at which the cleanup should be invoked.`
                  : `Cron expression at which the cleanup should be invoked.`
              }
            >
              <Input
                disabled={isDisable}
                placeholder="0 0 * * *"
                required
                value={cleanupConfig?.schedule?.toString() ?? ''}
                onChange={(ev) => {
                  onChange({
                    ...cleanupConfig,
                    schedule: ev.target.value,
                  });
                }}
              />
            </FieldWrapper>

            <div>
              <DropdownButton
                disabled={isDisable}
                items={crons.map((cron) => (
                  <DropdownMenu.Item
                    key={cron.value}
                    onSelect={() => {
                      onChange({
                        ...cleanupConfig,
                        schedule: cron.value,
                      });
                    }}
                  >
                    <Text as="p" weight="medium" className="whitespace-nowrap">
                      {cron.label}
                    </Text>
                    <Text as="p">{cron.value}</Text>
                  </DropdownMenu.Item>
                ))}
              >
                Frequent Frequencies
              </DropdownButton>
            </div>
            <Analytics
              name="open-adv-setting-event-log-cleanup"
              passHtmlAttributesToChildren
            >
              <Collapsible
                triggerChildren={<Heading size="4">Advanced Settings</Heading>}
              >
                <FieldWrapper
                  noErrorPlaceholder
                  label="Timeout (seconds)"
                  tooltip={
                    isDisable
                      ? `Enable event log cleanup to configure. Timeout for the query (in seconds, default: 60)`
                      : `Timeout for the query (in seconds, default: 60)`
                  }
                >
                  <Input
                    type="number"
                    min={1}
                    disabled={isDisable}
                    placeholder="60"
                    value={cleanupConfig?.timeout?.toString() ?? ''}
                    onChange={(ev) => {
                      onChange({
                        ...cleanupConfig,
                        timeout: safeParseInt(ev.target.value, undefined),
                      });
                    }}
                  />
                </FieldWrapper>

                <FieldWrapper
                  noErrorPlaceholder
                  label="Batch Size"
                  tooltip={
                    isDisable
                      ? `Enable event log cleanup to configure. Number of event trigger logs to delete in a batch (default: 10,000)`
                      : `Number of event trigger logs to delete in a batch (default: 10,000)`
                  }
                >
                  <Input
                    type="number"
                    min={1}
                    disabled={isDisable}
                    placeholder="10000"
                    value={cleanupConfig?.batch_size?.toString() ?? ''}
                    onChange={(ev) => {
                      onChange({
                        ...cleanupConfig,
                        batch_size: safeParseInt(ev.target.value, undefined),
                      });
                    }}
                  />
                </FieldWrapper>
              </Collapsible>
            </Analytics>
          </Flex>
        </ETAutoCleanupWrapper>
      </Collapsible>
    </Analytics>
  );
};
