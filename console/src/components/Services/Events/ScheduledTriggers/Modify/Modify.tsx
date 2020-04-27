import React from 'react';
import Endpoints from '../../../../../Endpoints';
import Button from '../../../../Common/Button/Button';
import { useScheduledTriggerAdd, defaultCronExpr } from '../Add/state';
import { ScheduledTrigger } from '../../Types';
import {
  getConfirmation,
  getEventTargetValue,
} from '../../../../Common/utils/jsUtils';
import { parseServerScheduledTrigger } from '../utils';
import styles from '../../Events.scss';
import Tooltip from '../../../../Common/Tooltip/Tooltip';
import Helmet from 'react-helmet';
import Modal from '../../../../Common/Modal/Modal';
import CronBuilder from '../../../../Common/CronBuilder/CronBuilder';
import AceEditor from '../../../../Common/AceEditor/BaseEditor';
import Headers from '../../../../Common/Headers/Headers';
import { saveScheduledTrigger, deleteScheduledTrigger } from '../../ServerIO';
import { getCreateScheduledEventQuery } from '../../../../Common/utils/v1QueryUtils';

type ModifyProps = {
  dispatch: any;
  currentTrigger?: ScheduledTrigger;
};

const invokeTrigger = (triggerName: string) => {
  const query = getCreateScheduledEventQuery(triggerName);
  return fetch(Endpoints.query, {
    method: 'POST',
    body: JSON.stringify(query),
  })
    .then(r => {
      if (r.status === 200) {
        return Promise.resolve();
      }
      return Promise.reject();
    })
    .catch(e => {
      console.error(e);
      return Promise.reject();
    });
};

const Modify = (props: ModifyProps) => {
  const [invokeButtonText, setInvokeButtonText] = React.useState('Invoke');
  const { dispatch, currentTrigger } = props;
  const { state, setState } = useScheduledTriggerAdd();
  const [cronBuilderState, setCronBuilderState] = React.useState(
    defaultCronExpr
  );

  React.useEffect(() => {
    if (currentTrigger) {
      const initState = parseServerScheduledTrigger(currentTrigger);
      setState.bulk(initState);
    }
  }, [currentTrigger]);

  const {
    name,
    webhook,
    schedule,
    payload,
    headers,
    loading,
    showScheduleModal,
  } = state;
  console.log(state);

  React.useEffect(() => {
    setCronBuilderState(schedule.value || defaultCronExpr);
  }, [schedule.value]);

  if (!currentTrigger) {
    return null;
  }

  const deleteFunc = () => {
    const isOk = getConfirmation('Are you sure?');
    if (!isOk) return;
    dispatch(deleteScheduledTrigger(currentTrigger));
  };

  const setName = (e: React.BaseSyntheticEvent) =>
    setState.name(getEventTargetValue(e));
  const setWebhookValue = (e: React.BaseSyntheticEvent) =>
    setState.setWebhookValue(getEventTargetValue(e));
  const setTriggerType = (e: React.BaseSyntheticEvent) =>
    setState.scheduleType(getEventTargetValue(e));
  const setScheduleValue = (e: React.BaseSyntheticEvent) =>
    setState.scheduleValue(getEventTargetValue(e));

  const sectionize = (section: JSX.Element) => (
    <div className={styles.add_mar_bottom}>{section}</div>
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
            message={'Name of the trigger'}
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
            message={'The HTTP endpoint that must be triggered'}
            className={styles.add_mar_left_mid}
          />
        </h2>
        <input
          type="text"
          placeholder="webhook"
          className={`form-control ${styles.inputWidthLarge}`}
          value={webhook.value}
          onChange={setWebhookValue}
        />
      </React.Fragment>
    );

  const getTriggerTypeInput = () => {
    return sectionize(
      <React.Fragment>
        <h2
          className={`${styles.subheading_text} ${styles.add_mar_bottom_small}`}
        >
          Trigger Type
          <Tooltip
            id="trigger-type"
            message={
              'Scheduled triggers could be scheduled with a cron and adhoc triggers can be invoked manually'
            }
            className={styles.add_mar_left_mid}
          />
        </h2>
        <select
          className={`form-control ${styles.inputWidthLarge}`}
          value={schedule.type}
          onChange={setTriggerType}
        >
          <option key="cron" value="cron">
            Cron
          </option>
          <option key="adhoc" value="adhoc">
            Adhoc
          </option>
        </select>
      </React.Fragment>
    );
  };

  const getScheduleInput = () => {
    if (schedule.type === 'adhoc') return null;

    const getScheduleInputText = (disabled: boolean, className: string) => (
      <input
        type="text"
        placeholder={defaultCronExpr}
        className={`form-control ${styles.inputWidthLarge} ${className}`}
        value={schedule.value}
        onChange={setScheduleValue}
        disabled={disabled}
      />
    );

    const modalOnSubmit = () => {
      setState.scheduleValue(cronBuilderState);
      setState.toggleScheduleModal();
    };
    const modalReset = () => {
      setCronBuilderState(schedule.value || '');
      setState.toggleScheduleModal();
    };
    const modal = (
      <Modal
        show={!!showScheduleModal}
        title="Build your CRON expression"
        onClose={modalReset}
        onCancel={modalReset}
        onSubmit={modalOnSubmit}
        submitText={'Use'}
      >
        <div>
          <CronBuilder
            value={cronBuilderState}
            showResultText
            showResultCron
            onChange={setCronBuilderState}
          />
        </div>
      </Modal>
    );
    return sectionize(
      <React.Fragment>
        <h2
          className={`${styles.subheading_text} ${styles.add_mar_bottom_small}`}
        >
          Cron schedule
          <Tooltip
            id="trigger-schedule"
            message={'Schedule for your cron'}
            className={styles.add_mar_left_mid}
          />
        </h2>
        <div className={`${styles.add_mar_bottom_mid} ${styles.display_flex}`}>
          {getScheduleInputText(false, styles.add_mar_right_mid)}
          {modal}
          <a
            onClick={setState.toggleScheduleModal}
            className={styles.cursorPointer}
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
            message={'The request payload for the HTTP trigger'}
            className={styles.add_mar_left_mid}
          />
        </h2>
        <AceEditor
          mode="json"
          value={payload}
          onChange={setState.payload}
          height={'200px'}
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
            message={'Configure headers for the request to the webhook'}
            className={styles.add_mar_left_mid}
          />
        </h2>
        <Headers headers={headers} setHeaders={setState.headers} />
      </React.Fragment>
    );

  const getActionButtons = () => {
    const callback = () => setState.loading(false);
    const onSave = (e: React.SyntheticEvent) => {
      e.preventDefault();
      setState.loading(true);
      dispatch(saveScheduledTrigger(state, currentTrigger, callback, callback));
    };

    const invoke = () => {
      setInvokeButtonText('Invoking...');
      invokeTrigger(currentTrigger.name)
        .then(() => {
          setInvokeButtonText('Invoke');
        })
        .catch(() => {
          setInvokeButtonText('Invoke');
        });
    };

    return (
      <div>
        <Button
          onClick={onSave}
          color="yellow"
          size="sm"
          disabled={loading}
          className={`${styles.add_mar_right}`}
        >
          {loading ? 'Saving...' : 'Save'}
        </Button>
        {currentTrigger.schedule_conf.type === 'adhoc' && (
          <Button
            onClick={invoke}
            color="white"
            size="sm"
            disabled={loading}
            className={`${styles.add_mar_right}`}
          >
            {invokeButtonText}
          </Button>
        )}
        <Button
          onClick={deleteFunc}
          color="red"
          size="sm"
          disabled={loading}
          className={`${styles.add_mar_right}`}
        >
          {loading ? 'Deleting...' : 'Delete'}
        </Button>
      </div>
    );
  };

  return (
    <div>
      <Helmet title={`${currentTrigger.name} | Scheduled Triggers | Modify`} />
      {getNameInput()}
      {getWebhookInput()}
      {getTriggerTypeInput()}
      {getScheduleInput()}
      {getPayloadInput()}
      {getHeadersInput()}
      <hr />
      {getActionButtons()}
    </div>
  );

  return (
    <div>
      <Button onClick={deleteFunc} color="red" size="sm">
        Delete
      </Button>
    </div>
  );
};

export default Modify;
