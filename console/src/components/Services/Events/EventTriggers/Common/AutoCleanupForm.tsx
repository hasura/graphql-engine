import defaultState from '@/components/Services/Events/EventTriggers/state';
import { Collapsible } from '@/new-components/Collapsible';
import { DropdownButton } from '@/new-components/DropdownButton';
import { InputSection } from '@/new-components/InputSetionWithoutForm';
import { Switch } from '@/new-components/Switch';
import React from 'react';
import { EventTriggerAutoCleanup } from '../../types';

interface AutoCleanupFormProps {
  cleanupConfig: EventTriggerAutoCleanup;
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
  return (
    <div className="w-1/2">
      <Collapsible
        triggerChildren={
          <h2 className="text-lg font-semibold mb-xs flex items-center mb-0">
            Auto-cleanup Event Logs
          </h2>
        }
      >
        <div className="flex items-center mb-sm">
          <Switch
            checked={!cleanupConfig.paused}
            onCheckedChange={() => {
              onChange({
                ...cleanupConfig,
                paused: !cleanupConfig.paused,
              });
            }}
          />
          <span className="ml-xs cursor-pointer">Enable event log cleanup</span>
        </div>
        <div className="flex items-center mb-sm">
          <Switch
            checked={cleanupConfig.clean_invocation_logs}
            disabled={cleanupConfig.paused}
            onCheckedChange={() => {
              onChange({
                ...cleanupConfig,
                clean_invocation_logs: !cleanupConfig.clean_invocation_logs,
              });
            }}
          />
          <span className="ml-xs cursor-pointer">
            Clean invocation logs with event logs
          </span>
        </div>
        <InputSection
          label="Clear logs older than (hours)"
          tooltip="Clear event logs older than (in hours, default:168 hours or 7
            days)"
          placeholder={
            defaultState.cleanupConfig.clear_older_than?.toString() || ''
          }
          value={cleanupConfig.clear_older_than?.toString() || ''}
          onChange={value => {
            onChange({
              ...cleanupConfig,
              clear_older_than: value ? parseInt(value, 10) : undefined,
            });
          }}
        />
        <InputSection
          label="Cleanup Frequency"
          tooltip="Cron expression at which the cleanup should be invoked."
          placeholder={defaultState.cleanupConfig.timeout?.toString() || ''}
          value={cleanupConfig.schedule?.toString() || ''}
          onChange={value => {
            onChange({
              ...cleanupConfig,
              schedule: value,
            });
          }}
        />

        <div className="my-sm">
          <DropdownButton
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
                  className="cursor-pointer mx-1 px-xs py-1 rounded hover:bg-gray-100"
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
        <Collapsible
          triggerChildren={
            <h2 className="text-lg font-semibold mb-xs flex items-center mb-0">
              Advanced Settings
            </h2>
          }
        >
          <InputSection
            label="Timeout (seconds)"
            tooltip="Timeout for the query (in seconds, default: 60)"
            placeholder={defaultState.cleanupConfig.timeout?.toString() || ''}
            value={cleanupConfig.timeout?.toString() || ''}
            onChange={value => {
              onChange({
                ...cleanupConfig,
                timeout: value ? parseInt(value, 10) : undefined,
              });
            }}
          />

          <InputSection
            label="Batch Size"
            tooltip="Number of event trigger logs to delete in a batch (default: 10,000)"
            placeholder={
              defaultState.cleanupConfig.batch_size?.toString() || ''
            }
            value={cleanupConfig.batch_size?.toString() || ''}
            onChange={value => {
              onChange({
                ...cleanupConfig,
                batch_size: value ? parseInt(value, 10) : undefined,
              });
            }}
          />
        </Collapsible>
      </Collapsible>
    </div>
  );
};
