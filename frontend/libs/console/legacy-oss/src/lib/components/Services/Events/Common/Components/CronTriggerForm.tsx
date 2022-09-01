import React from 'react';
// import DateTimePicker from 'react-datetime';
// import { Moment } from 'moment';
import './ReactDateTime.css';
import { useScheduledTrigger, defaultCronExpr } from '../../CronTriggers/state';
import AceEditor from '../../../../Common/AceEditor/BaseEditor';
import Toggle from '../../../../Common/Toggle/Toggle';
import CollapsibleToggle from '../../../../Common/CollapsibleToggle/CollapsibleToggle';
import Headers from '../../../../Common/Headers/Headers';
import RetryConf from './RetryConfEditor';
import FrequentlyUsedCrons from './FrequentlyUsedCrons';
import FormSection from './FormSection';
import { heading, inputStyles } from '../../constants';

type Props = ReturnType<typeof useScheduledTrigger>;

const Form: React.FC<Props> = props => {
  const { state, setState } = props;

  const {
    name,
    webhook,
    schedule,
    payload,
    headers,
    comment,
    includeInMetadata,
  } = state;

  const setName = (e: React.ChangeEvent<HTMLInputElement>) =>
    setState.name(e.target.value);
  const setWebhookValue = (e: React.ChangeEvent<HTMLInputElement>) =>
    setState.webhook(e.target.value);
  const setScheduleValue = (e: React.ChangeEvent<HTMLInputElement>) =>
    setState.schedule(e.target.value);
  const setComment = (e: React.ChangeEvent<HTMLInputElement>) =>
    setState.comment(e.target.value);

  return (
    <React.Fragment>
      <FormSection
        heading="Name"
        tooltip="Name of the trigger"
        id="trigger-name"
      >
        <input
          type="text"
          placeholder="name"
          className={`${inputStyles} w-80`}
          value={name}
          onChange={setName}
        />
      </FormSection>
      <FormSection
        heading="Webhook"
        tooltip="The HTTP URL that should be triggered. You can also provide the URL from environment variables, e.g. {{MY_WEBHOOK_URL}}"
        id="trigger-webhook"
      >
        <input
          type="text"
          placeholder="http://httpbin.org/post"
          className={`${inputStyles} w-80`}
          value={webhook}
          onChange={setWebhookValue}
        />
      </FormSection>
      <FormSection
        id="trigger-schedule"
        tooltip="Schedule for your cron (events are created based on the UTC timezone)"
        heading="Cron Schedule"
      >
        <div className="mb-5 flex flex-row items-center">
          <input
            type="text"
            placeholder={defaultCronExpr}
            className={`${inputStyles} w-80 mr-5`}
            value={schedule}
            onChange={setScheduleValue}
          />
          <a
            className="cursor-pointer"
            href="https://crontab.guru/#*_*_*_*_*"
            target="_blank"
            rel="noopener noreferrer"
          >
            Build a cron expression
          </a>
        </div>
        <FrequentlyUsedCrons setCron={setState.schedule} />
      </FormSection>
      <FormSection
        id="trigger-payload"
        tooltip="The request payload for the HTTP trigger"
        heading="Payload"
      >
        <AceEditor
          mode="json"
          value={payload}
          onChange={setState.payload}
          height="200px"
        />
      </FormSection>
      <CollapsibleToggle
        title={<h2 className={heading}>Advanced</h2>}
        testId="advanced-configuration"
      >
        <FormSection
          id="trigger-headers"
          heading="Headers"
          tooltip="Configure headers for the request to the webhook"
        >
          <Headers headers={headers} setHeaders={setState.headers} />
        </FormSection>
        <FormSection
          id="trigger-retry-conf"
          heading="Retry configuration"
          tooltip="Retry configuration if the call to the webhook fails"
        >
          <RetryConf
            retryConf={state.retryConf}
            setRetryConf={setState.retryConf}
          />
        </FormSection>
        <FormSection
          id="trigger-include-in-metadata"
          heading="Include in Metadata"
          tooltip="If enabled, this cron trigger will be included in the metadata of GraphqL Engine i.e. it will be a part of the metadata that is exported as migrations."
        >
          <div className="flex mr-xs">
            <Toggle
              checked={includeInMetadata}
              onChange={setState.toggleIncludeInMetadata}
              className="mr-xs"
              icons={false}
            />
            <span>Include this trigger in Hasura Metadata</span>
          </div>
        </FormSection>
        <FormSection
          id="trigger-comment"
          heading="Comment"
          tooltip="Description of your cron trigger"
        >
          <input
            type="text"
            placeholder="comment"
            className={`${inputStyles} mr-2.5 w-4/12`}
            value={comment || ''}
            onChange={setComment}
          />
        </FormSection>
      </CollapsibleToggle>
    </React.Fragment>
  );
};

export default Form;
