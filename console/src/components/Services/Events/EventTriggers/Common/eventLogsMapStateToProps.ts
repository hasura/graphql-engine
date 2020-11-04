import { connect, ConnectedProps } from 'react-redux';
import { MapStateToProps, Dispatch } from '../../../../../types';
import { RouterTriggerProps, EventTrigger } from '../../types';
import { NotFoundError } from '../../../../Error/PageNotFound';
import { getEventTriggerByName } from '../../../../../metadata/selector';

type PropsFromState = {
  triggerName: string;
  currentTrigger: EventTrigger;
  readOnlyMode: boolean;
  currentSource: string;
};

const mapStateToProps: MapStateToProps<PropsFromState, RouterTriggerProps> = (
  state,
  ownProps
) => {
  const triggerName = ownProps.params.triggerName;
  const currentTrigger = getEventTriggerByName(state)(triggerName);

  if (!currentTrigger) {
    // throw a 404 exception
    throw new NotFoundError();
  }

  return {
    triggerName,
    currentTrigger,
    readOnlyMode: state.main.readOnlyMode,
    currentSource: state.tables.currentDataSource,
  };
};

const mapDispatchToProps = (dispatch: Dispatch) => ({ dispatch });

export const connector = connect(mapStateToProps, mapDispatchToProps);

export type EventsLogsInjectedProps = ConnectedProps<typeof connector>;
