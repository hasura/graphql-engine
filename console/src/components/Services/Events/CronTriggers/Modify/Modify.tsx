import React from 'react';
import Button from '../../../../Common/Button/Button';
import { useScheduledTrigger } from '../state';
import { ScheduledTrigger } from '../../types';
import { getConfirmation } from '../../../../Common/utils/jsUtils';
import { Dispatch } from '../../../../../types';
import { parseServerScheduledTrigger } from '../utils';
import styles from '../../Events.scss';
import CronTriggerFrom from '../../Common/Components/CronTriggerForm';
import { saveScheduledTrigger, deleteScheduledTrigger } from '../../ServerIO';

type Props = {
  dispatch: Dispatch;
  currentTrigger?: ScheduledTrigger;
};

const Modify: React.FC<Props> = props => {
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

  const requestCallback = () => setState.loading(false);
  const deleteFunc = () => {
    const isOk = getConfirmation('Are you sure?');
    if (!isOk) return;
    setState.loading(true);
    dispatch(
      deleteScheduledTrigger(currentTrigger, requestCallback, requestCallback)
    );
  };

  const onSave = (e: React.SyntheticEvent) => {
    e.preventDefault();
    setState.loading(true);
    dispatch(
      saveScheduledTrigger(
        state,
        currentTrigger,
        requestCallback,
        requestCallback
      )
    );
  };

  return (
    <div className={styles.add_mar_bottom}>
      <CronTriggerFrom state={state} setState={setState} />
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
    </div>
  );
};

export default Modify;
