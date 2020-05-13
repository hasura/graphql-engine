import React from 'react';
import DateTimePicker from 'react-datetime';
import { Moment } from 'moment';
import { useAdhocEventAdd } from './state';
import styles from '../../Events.scss';
import Button from '../../../../Common/Button/Button';
import AceEditor from '../../../../Common/AceEditor/BaseEditor';
import Headers from '../../../../Common/Headers/Headers';
import RetryConfEditor from '../../Common/Components/RetryConfEditor';
import { getEventTargetValue } from '../../../../Common/utils/jsUtils';
import Tooltip from '../../../../Common/Tooltip/Tooltip';
import { createScheduledEvent } from '../../ServerIO';

type Props = {
  dispatch: any;
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

  const setWebhookValue = (e: React.BaseSyntheticEvent) =>
    setState.webhook(getEventTargetValue(e));
  const setTimeValue = (e: Moment | string) => {
    if (typeof e !== 'string') {
      setState.time(e.toDate());
    }
  };
  const setComment = (e: React.BaseSyntheticEvent) =>
    setState.comment(getEventTargetValue(e));

  const sectionize = (section: JSX.Element) => (
    <div className={styles.add_mar_bottom}>
      {section}
      <hr />
    </div>
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

  const getTimeInput = () => {
    return sectionize(
      <React.Fragment>
        <h2
          className={`${styles.subheading_text} ${styles.add_mar_bottom_small}`}
        >
          Time
          <Tooltip
            id="trigger-schedule"
            message="The time that this event must be delivered"
            className={styles.add_mar_left_mid}
          />
        </h2>
        <div className={`${styles.add_mar_bottom_mid} ${styles.display_flex}`}>
          <DateTimePicker
            value={time}
            onChange={setTimeValue}
            inputProps={{
              className: `form-control ${styles.inputWidthLarge}`,
            }}
          />
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
        <RetryConfEditor
          retryConf={retryConf}
          setRetryConf={setState.retryConf}
        />
      </div>
    );
  };

  const getCommentInput = () => {
    return sectionize(
      <div className={styles.add_mar_bottom}>
        <h2 className={`${styles.subheading_text}`}>
          Comment
          <Tooltip
            id="trigger-comment"
            message="Description of this event"
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

  const save = () => {
    setState.loading(true);
    const succesCallback = () => setState.bulk();
    const errorCallback = () => setState.loading(false);
    dispatch(createScheduledEvent(state, succesCallback, errorCallback));
  };

  return (
    <div className={styles.add_mar_bottom}>
      {getWebhookInput()}
      {getTimeInput()}
      {getPayloadInput()}
      {getHeadersInput()}
      {getRetryConfInput()}
      {getCommentInput()}
      <Button size="s" color="yellow" onClick={save} disabled={loading}>
        {loading ? 'Creating...' : 'Create scheduled event'}
      </Button>
    </div>
  );
};

export default Add;
