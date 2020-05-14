import React from 'react';
// import DateTimePicker from 'react-datetime';
// import { Moment } from 'moment';
import './ReactDateTime.css';
import {
  useScheduledTrigger,
  defaultCronExpr,
} from '../../ScheduledTriggers/state';
import AceEditor from '../../../../Common/AceEditor/BaseEditor';
import Toggle from '../../../../Common/Toggle/Toggle';
import CollapsibleToggle from '../../../../Common/CollapsibleToggle/CollapsibleToggle';
import styles from '../../Events.scss';
import Tooltip from '../../../../Common/Tooltip/Tooltip';
import Headers from '../../../../Common/Headers/Headers';
import RetryConf from './RetryConfEditor';

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

  const sectionize = (section: JSX.Element) => (
    <div className={styles.add_mar_bottom}>
      {section}
      <hr />
    </div>
  );

  const getNameInput = () =>
    sectionize(
      <React.Fragment>
        <h2
          className={`${styles.subheading_text} ${styles.add_mar_bottom_small}`}
        >
          Name
          <Tooltip
            id="trigger-name"
            message="Name of the trigger"
            className={styles.add_mar_left_mid}
          />
        </h2>
        <input
          type="text"
          placeholder="name"
          className={`form-control ${styles.inputWidthLarge}`}
          value={name}
          onChange={setName}
        />
      </React.Fragment>
    );

  const getWebhookInput = () =>
    sectionize(
      <React.Fragment>
        <h2
          className={`${styles.subheading_text} ${styles.add_mar_bottom_small}`}
        >
          Webhook
          <Tooltip
            id="trigger-webhook"
            message="The HTTP endpoint that must be triggered"
            className={styles.add_mar_left_mid}
          />
        </h2>
        <input
          type="text"
          placeholder="http://httpbin.org/post"
          className={`form-control ${styles.inputWidthLarge}`}
          value={webhook}
          onChange={setWebhookValue}
        />
      </React.Fragment>
    );

  const getScheduleInput = () => {
    return sectionize(
      <React.Fragment>
        <h2
          className={`${styles.subheading_text} ${styles.add_mar_bottom_small}`}
        >
          Cron schedule
          <Tooltip
            id="trigger-schedule"
            message="Schedule for your cron"
            className={styles.add_mar_left_mid}
          />
        </h2>
        <div className={`${styles.add_mar_bottom_mid} ${styles.display_flex}`}>
          <input
            type="text"
            placeholder={defaultCronExpr}
            className={`form-control ${styles.inputWidthLarge} ${styles.add_mar_right_mid}`}
            value={schedule}
            onChange={setScheduleValue}
          />
          <a
            className={styles.cursorPointer}
            href="https://crontab.guru/#*_*_*_*_*"
            target="_blank"
            rel="noopener noreferrer"
          >
            Build a cron expression
          </a>
        </div>
      </React.Fragment>
    );
  };

  // TODO make JSONEditor component
  const getPayloadInput = () =>
    sectionize(
      <React.Fragment>
        <h2
          className={`${styles.subheading_text} ${styles.add_mar_bottom_small}`}
        >
          Payload
          <Tooltip
            id="trigger-payload"
            message="The request payload for the HTTP trigger"
            className={styles.add_mar_left_mid}
          />
        </h2>
        <AceEditor
          mode="json"
          value={payload}
          onChange={setState.payload}
          height="200px"
        />
      </React.Fragment>
    );

  const getHeadersInput = () =>
    sectionize(
      <React.Fragment>
        <h2
          className={`${styles.subheading_text} ${styles.add_mar_bottom_small}`}
        >
          Headers
          <Tooltip
            id="trigger-headers"
            message="Configure headers for the request to the webhook"
            className={styles.add_mar_left_mid}
          />
        </h2>
        <Headers headers={headers} setHeaders={setState.headers} />
      </React.Fragment>
    );

  const getCommentInput = () => {
    return sectionize(
      <div className={styles.add_mar_bottom}>
        <h2 className={`${styles.subheading_text}`}>
          Comment
          <Tooltip
            id="trigger-comment"
            message="Description of your cron trigger"
            className={styles.add_mar_left_mid}
          />
        </h2>
        <input
          type="text"
          placeholder="comment"
          className={`form-control ${styles.inputWidthLarge} ${styles.add_mar_right_mid}`}
          value={comment || ''}
          onChange={setComment}
        />
      </div>
    );
  };

  const getMetadataIncludeCheckbox = () => {
    return sectionize(
      <div className={styles.add_mar_bottom}>
        <h2 className={`${styles.subheading_text}`}>
          Include in Metadata
          <Tooltip
            id="trigger-comment"
            message="If enabled, this cron trigger will be included in the metadata of GraphqL Engine"
            className={styles.add_mar_left_mid}
          />
        </h2>
        <div className={`${styles.display_flex} ${styles.add_mar_right_mid}`}>
          <Toggle
            checked={includeInMetadata}
            onChange={setState.toggleIncludeInMetadata}
            className={styles.add_mar_right_mid}
            icons={false}
          />
          <span>Include this trigger in Hasura Metadata</span>
        </div>
      </div>
    );
  };

  const getRetryConfInput = () => {
    return sectionize(
      <div className={styles.add_mar_bottom}>
        <h2 className={`${styles.subheading_text}`}>
          Retry configuration
          <Tooltip
            id="trigger-headers"
            message="Retry configuration if the call to the webhook fails"
            className={styles.add_mar_left_mid}
          />
        </h2>
        <RetryConf
          retryConf={state.retryConf}
          setRetryConf={setState.retryConf}
        />
      </div>
    );
  };

  const getAdvancedSection = () => {
    const label = <h2 className={styles.subheading_text}>Advanced</h2>;
    return sectionize(
      <CollapsibleToggle title={label}>
        {getHeadersInput()}
        {getRetryConfInput()}
        {getMetadataIncludeCheckbox()}
        {getCommentInput()}
      </CollapsibleToggle>
    );
  };

  return (
    <React.Fragment>
      {getNameInput()}
      {getWebhookInput()}
      {getScheduleInput()}
      {getPayloadInput()}
      {getAdvancedSection()}
    </React.Fragment>
  );
};

export default Form;
