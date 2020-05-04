import React from 'react';
import Helmet from 'react-helmet';
import Button from '../../../../Common/Button/Button';
import { useScheduledTrigger } from '../state';
import { ScheduledTrigger } from '../../Types';
import { getConfirmation } from '../../../../Common/utils/jsUtils';
import { parseServerScheduledTrigger } from '../utils';
import styles from '../../Events.scss';
import ScheduledTriggerForm from '../../Common/Components/ScheduledTriggerForm';
import {
  saveScheduledTrigger,
  invokeScheduledTrigger,
  deleteScheduledTrigger,
} from '../../ServerIO';

type ModifyProps = {
  dispatch: any;
  currentTrigger?: ScheduledTrigger;
};

const Modify = (props: ModifyProps) => {
  const [invokeButtonText, setInvokeButtonText] = React.useState('Invoke');
  const { dispatch, currentTrigger } = props;
  const { state, setState } = useScheduledTrigger();

  React.useEffect(() => {
    if (currentTrigger) {
      const initState = parseServerScheduledTrigger(currentTrigger);
      setState.bulk(initState);
    }
  }, [currentTrigger]);

  if (!currentTrigger) {
    return null;
  }

  const deleteFunc = () => {
    const isOk = getConfirmation('Are you sure?');
    if (!isOk) return;
    dispatch(deleteScheduledTrigger(currentTrigger));
  };

  const getActionButtons = () => {
    const callback = () => setState.loading(false);
    const onSave = (e: React.SyntheticEvent) => {
      e.preventDefault();
      setState.loading(true);
      dispatch(saveScheduledTrigger(state, currentTrigger, callback, callback));
    };

    const invoke = () => {
      setInvokeButtonText('Invoking...');
      const successCallback = () => {
        setInvokeButtonText('Done');
        setTimeout(() => {
          setInvokeButtonText('Invoke');
        }, 1000);
      };
      const errorCallback = () => {
        setInvokeButtonText('Error');
        setTimeout(() => {
          setInvokeButtonText('Invoke');
        }, 1000);
      };
      dispatch(
        invokeScheduledTrigger(
          currentTrigger.name,
          successCallback,
          errorCallback
        )
      );
    };

    return (
      <div>
        <Button
          onClick={onSave}
          color="yellow"
          size="sm"
          disabled={state.loading}
          className={`${styles.add_mar_right}`}
        >
          {state.loading ? 'Saving...' : 'Save'}
        </Button>
        {currentTrigger.schedule_conf.type === 'adhoc' && (
          <Button
            onClick={invoke}
            color="white"
            size="sm"
            disabled={state.loading}
            className={`${styles.add_mar_right}`}
          >
            {invokeButtonText}
          </Button>
        )}
        <Button
          onClick={deleteFunc}
          color="red"
          size="sm"
          disabled={state.loading}
          className={`${styles.add_mar_right}`}
        >
          Delete
        </Button>
      </div>
    );
  };

  return (
    <div>
      <Helmet title={`${currentTrigger.name} | Scheduled Triggers | Modify`} />
      <ScheduledTriggerForm state={state} setState={setState} />
      <hr />
      {getActionButtons()}
    </div>
  );
};

export default Modify;
