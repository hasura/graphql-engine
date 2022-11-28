import React from 'react';
import Helmet from 'react-helmet';
import { connect, ConnectedProps } from 'react-redux';
import { Analytics, REDACT_EVERYTHING } from '@/features/Analytics';
import { Button } from '@/new-components/Button';
import { useScheduledTrigger, LocalScheduledTriggerState } from '../state';
import CronTriggerFrom from '../../Common/Components/CronTriggerForm';
import {
  getReactHelmetTitle,
  mapDispatchToPropsEmpty,
} from '../../../../Common/utils/reactUtils';
import { MapStateToProps } from '../../../../../types';
import { addScheduledTrigger } from '../../ServerIO';
import { EVENTS_SERVICE_HEADING, CRON_TRIGGER } from '../../constants';

interface Props extends InjectedProps {
  initState?: LocalScheduledTriggerState;
}

const Main: React.FC<Props> = props => {
  const { dispatch, initState, readOnlyMode } = props;
  const { state, setState } = useScheduledTrigger(initState);

  const callback = () => setState.loading('add', false);
  const onSave = (e: React.SyntheticEvent) => {
    e.preventDefault();
    setState.loading('add', true);
    dispatch(addScheduledTrigger(state, callback, callback));
  };

  return (
    <Analytics name="AddScheduledTrigger" {...REDACT_EVERYTHING}>
      <div className="md-md">
        <Helmet
          title={getReactHelmetTitle(
            `Create ${CRON_TRIGGER}`,
            EVENTS_SERVICE_HEADING
          )}
        />
        <div className="font-bold mb-xl text-[18px] pb-0">
          Create a cron trigger
        </div>
        <CronTriggerFrom state={state} setState={setState} />
        {!readOnlyMode && (
          <div className="mr-xl">
            <Analytics
              name="events-tab-button-create-cron-trigger"
              passHtmlAttributesToChildren
            >
              <Button
                isLoading={state.loading.add}
                loadingText="Creating..."
                onClick={onSave}
                mode="primary"
                disabled={state.loading.add}
              >
                Create
              </Button>
            </Analytics>
          </div>
        )}
      </div>
    </Analytics>
  );
};

type PropsFromState = {
  readOnlyMode: boolean;
};
const mapStateToProps: MapStateToProps<PropsFromState> = state => ({
  readOnlyMode: state.main.readOnlyMode,
});

const connector = connect(mapStateToProps, mapDispatchToPropsEmpty);
type InjectedProps = ConnectedProps<typeof connector>;

const AddConnector = connector(Main);
export default AddConnector;
