import React from 'react';
import { useQueryClient } from 'react-query';
import { Button } from '@/new-components/Button';
import { useScheduledTrigger } from '../state';
import { ScheduledTrigger } from '../../types';
import { Dispatch } from '../../../../../types';
import { parseServerScheduledTrigger } from '../utils';
import CronTriggerFrom from '../../Common/Components/CronTriggerForm';
import { saveScheduledTrigger, deleteScheduledTrigger } from '../../ServerIO';

type Props = {
  dispatch: Dispatch;
  currentTrigger?: ScheduledTrigger;
};

const Modify: React.FC<Props> = props => {
  const { dispatch, currentTrigger } = props;
  const { state, setState } = useScheduledTrigger();
  const queryClient = useQueryClient();

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
    const requestCallback = () => {
      setState.loading('delete', false);
      queryClient.refetchQueries(['cronTrigger'], { active: true });
    };
    setState.loading('delete', true);
    dispatch(
      deleteScheduledTrigger(currentTrigger, requestCallback, requestCallback)
    );
  };

  const onSave = (e: React.SyntheticEvent) => {
    e.preventDefault();
    const requestCallback = () => {
      setState.loading('modify', false);
    };
    setState.loading('modify', true);
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
    <div className="mb-md">
      <CronTriggerFrom state={state} setState={setState} />
      <div className="flex">
        <div className="mr-md">
          <Button
            onClick={onSave}
            mode="primary"
            disabled={state.loading.modify}
            loadingText="Saving..."
            isLoading={state.loading.modify}
          >
            Save
          </Button>
        </div>
        <div className="mr-md">
          <Button
            onClick={deleteFunc}
            mode="destructive"
            disabled={state.loading.delete}
          >
            {state.loading.delete ? 'Deleting...' : 'Delete'}
          </Button>
        </div>
      </div>
    </div>
  );
};

export default Modify;
