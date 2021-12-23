import React from 'react';
import DateTimePicker from 'react-datetime';
import { Moment } from 'moment';
import { useAdhocEventAdd } from './state';
import styles from '../../Events.scss';
import Button from '../../../../Common/Button/Button';
import { Dispatch } from '../../../../../types';
import AceEditor from '../../../../Common/AceEditor/BaseEditor';
import Headers from '../../../../Common/Headers/Headers';
import RetryConfEditor from '../../Common/Components/RetryConfEditor';
import CollapsibleToggle from '../../../../Common/CollapsibleToggle/CollapsibleToggle';
import FormSection from '../../Common/Components/FormSection';
import { createScheduledEvent } from '../../ServerIO';

type Props = {
  dispatch: Dispatch;
};

const Add: React.FC<Props> = ({ dispatch }) => {
  const { state, setState } = useAdhocEventAdd();
  const {
    webhook,
    time,
    payload,
    headers,
    retryConf,
    comment,
    loading,
  } = state;

  const setWebhookValue = (e: React.ChangeEvent<HTMLInputElement>) =>
    setState.webhook(e.target.value);
  const setTimeValue = (e: Moment | string) => {
    if (typeof e !== 'string') {
      setState.time(e.toDate());
    }
  };
  const setComment = (e: React.ChangeEvent<HTMLInputElement>) =>
    setState.comment(e.target.value);

  const save = () => {
    setState.loading(true);
    const succesCallback = () => setState.bulk();
    const errorCallback = () => setState.loading(false);
    dispatch(createScheduledEvent(state, succesCallback, errorCallback));
  };

  return (
    <div className={styles.add_mar_bottom}>
      <FormSection
        id="event-webhook"
        tooltip="The HTTP endpoint that must be triggered"
        heading="Webhook"
      >
        <input
          type="text"
          placeholder="http://httpbin.org/post"
          data-test="one-off-webhook"
          className={`form-control ${styles.inputWidthLarge}`}
          value={webhook}
          onChange={setWebhookValue}
        />
      </FormSection>
      <FormSection
        heading="Time"
        tooltip="The time that this event must be delivered"
        id="event-time"
      >
        <div className={`${styles.add_mar_bottom_mid} ${styles.display_flex}`}>
          <DateTimePicker
            value={time}
            onChange={setTimeValue}
            inputProps={{
              className: `form-control ${styles.inputWidthLarge}`,
            }}
          />
        </div>
      </FormSection>
      <FormSection
        heading="Payload"
        tooltip="The request payload for the HTTP trigger"
        id="event-payload"
      >
        <AceEditor
          mode="json"
          value={payload}
          onChange={setState.payload}
          height="200px"
        />
      </FormSection>
      <CollapsibleToggle
        title={<h2 className={styles.subheading_text}>Advanced</h2>}
        testId="event-advanced-configuration"
      >
        <FormSection
          heading="Headers"
          tooltip="Configure headers for the request to the webhook"
          id="event-headers"
        >
          <Headers headers={headers} setHeaders={setState.headers} />
        </FormSection>
        <FormSection
          heading="Retry configuration"
          tooltip="Retry configuration if the call to the webhook fails"
          id="event-retry-conf"
        >
          <RetryConfEditor
            retryConf={retryConf}
            setRetryConf={setState.retryConf}
          />
        </FormSection>
        <FormSection
          heading="Comment"
          tooltip="Description of this event"
          id="event-retry-conf"
        >
          <input
            type="text"
            placeholder="comment"
            className={`form-control ${styles.inputWidthLarge} ${styles.add_mar_right_mid}`}
            value={comment || ''}
            onChange={setComment}
          />
        </FormSection>
      </CollapsibleToggle>
      <Button
        size="s"
        color="yellow"
        onClick={save}
        disabled={loading}
        data-test="create-schedule-event"
      >
        {loading ? 'Creating...' : 'Create scheduled event'}
      </Button>
    </div>
  );
};

export default Add;
