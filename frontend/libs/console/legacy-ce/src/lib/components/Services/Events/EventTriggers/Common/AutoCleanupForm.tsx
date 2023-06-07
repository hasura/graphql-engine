import { FaCircle } from 'react-icons/fa';
import { Analytics } from '../../../../../features/Analytics';
import { Tooltip } from '../../../../../new-components/Tooltip';
import { Collapsible } from '../../../../../new-components/Collapsible';
import { DropdownButton } from '../../../../../new-components/DropdownButton';
import { InputSection } from '../../../../../new-components/InputSetionWithoutForm';
import { Switch } from '../../../../../new-components/Switch';
import { EventTriggerAutoCleanup } from '../../types';
import { ETAutoCleanupWrapper } from '../../../../../features/EETrial';

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
          <h2 className="text-lg font-semibold mb-xs flex items-center mb-0">
            Auto-cleanup Event Logs
            <div className="flex items-center">
              <Tooltip
                side="top"
                tooltipContentChildren={
                  isCleanupConfigSet &&
                  !(
                    cleanupConfig?.paused &&
                    Object.keys(cleanupConfig).length === 1
                  )
                    ? 'Auto-cleanup has been configured. After clearing/resetting, save changes to remove the configuration.'
                    : 'Auto-cleanup is currently not configured'
                }
                className="h-full flex items-center"
              >
                {' '}
                {isCleanupConfigSet &&
                !(
                  cleanupConfig?.paused &&
                  Object.keys(cleanupConfig).length === 1
                ) ? (
                  <FaCircle className="ml-xs fill-sky-600 text-xs" />
                ) : (
                  <FaCircle className="ml-xs fill-slate-400 text-xs" />
                )}
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
              </Tooltip>
            </div>
          </h2>
        }
        defaultOpen
      >
        <ETAutoCleanupWrapper>
          <div className="w-1/2">
            <div className="flex items-center mb-sm">
              <Tooltip
                side="right"
                tooltipContentChildren={
                  isCleanupConfigSet
                    ? 'When not enabled, event log cleanup is paused. To completely remove event log cleanup configuration use Clear/Reset button'
                    : 'When not enabled, event log cleanup is paused'
                }
                className="flex items-center ml-0"
              >
                <Switch
                  checked={cleanupConfig?.paused === false}
                  onCheckedChange={() => {
                    onChange({
                      ...cleanupConfig,
                      paused: cleanupConfig?.paused === false ? true : false,
                    });
                  }}
                />
                <span className="ml-xs cursor-pointer">
                  Enable event log cleanup
                </span>
              </Tooltip>
            </div>
            {
              <div>
                <div className="flex items-center mb-sm">
                  <Tooltip
                    side="right"
                    tooltipContentChildren={
                      isDisable
                        ? 'Enable event log cleanup to configure'
                        : 'Enabling this will clear event invocation logs along with event logs'
                    }
                    className="flex items-center ml-0"
                  >
                    <Switch
                      checked={cleanupConfig?.clean_invocation_logs}
                      disabled={isDisable}
                      onCheckedChange={() => {
                        onChange({
                          ...cleanupConfig,
                          clean_invocation_logs:
                            !cleanupConfig?.clean_invocation_logs,
                        });
                      }}
                    />
                    <span className="ml-xs cursor-pointer">
                      Clean invocation logs with event logs
                    </span>
                  </Tooltip>
                </div>

                <InputSection
                  label="Clear logs older than (hours)"
                  disabled={isDisable}
                  tooltip={
                    isDisable
                      ? `Enable event log cleanup to configure. Clear event logs older than (in hours)`
                      : `Clear event logs older than (in hours)`
                  }
                  placeholder="168"
                  required
                  value={cleanupConfig?.clear_older_than?.toString() ?? ''}
                  onChange={value => {
                    onChange({
                      ...cleanupConfig,
                      clear_older_than: value ? parseInt(value, 10) : undefined,
                    });
                  }}
                />
                <InputSection
                  label="Cleanup Frequency"
                  disabled={isDisable}
                  tooltip={
                    isDisable
                      ? `Enable event log cleanup to configure. Cron expression at which the cleanup should be invoked.`
                      : `Cron expression at which the cleanup should be invoked.`
                  }
                  placeholder="0 0 * * *"
                  required
                  value={cleanupConfig?.schedule?.toString() ?? ''}
                  onChange={value => {
                    onChange({
                      ...cleanupConfig,
                      schedule: value,
                    });
                  }}
                />

                <div className="my-sm">
                  <DropdownButton
                    disabled={isDisable}
                    items={[
                      crons.map(cron => (
                        <div
                          key={cron.value}
                          onClick={() => {
                            onChange({
                              ...cleanupConfig,
                              schedule: cron.value,
                            });
                          }}
                          className="py-xs cursor-pointer mx-1 px-xs py-1 rounded hover:bg-gray-100"
                        >
                          <p className="mb-0 font-semibold whitespace-nowrap">
                            {cron.label}
                          </p>
                          <p className="mb-0">{cron.value}</p>
                        </div>
                      )),
                    ]}
                  >
                    <span className="font-bold">Frequent Frequencies</span>
                  </DropdownButton>
                </div>
                <Analytics
                  name="open-adv-setting-event-log-cleanup"
                  passHtmlAttributesToChildren
                >
                  <Collapsible
                    triggerChildren={
                      <h2 className="text-lg font-semibold mb-xs flex items-center mb-0">
                        Advanced Settings
                      </h2>
                    }
                  >
                    <InputSection
                      label="Timeout (seconds)"
                      disabled={isDisable}
                      tooltip={
                        isDisable
                          ? `Enable event log cleanup to configure. Timeout for the query (in seconds, default: 60)`
                          : `Timeout for the query (in seconds, default: 60)`
                      }
                      placeholder="60"
                      value={cleanupConfig?.timeout?.toString() ?? ''}
                      onChange={value => {
                        onChange({
                          ...cleanupConfig,
                          timeout: value ? parseInt(value, 10) : undefined,
                        });
                      }}
                    />

                    <InputSection
                      label="Batch Size"
                      disabled={isDisable}
                      tooltip={
                        isDisable
                          ? `Enable event log cleanup to configure. Number of event trigger logs to delete in a batch (default: 10,000)`
                          : `Number of event trigger logs to delete in a batch (default: 10,000)`
                      }
                      placeholder="10000"
                      value={cleanupConfig?.batch_size?.toString() ?? ''}
                      onChange={value => {
                        onChange({
                          ...cleanupConfig,
                          batch_size: value ? parseInt(value, 10) : undefined,
                        });
                      }}
                    />
                  </Collapsible>
                </Analytics>
              </div>
            }
          </div>
        </ETAutoCleanupWrapper>
      </Collapsible>
    </Analytics>
  );
};
