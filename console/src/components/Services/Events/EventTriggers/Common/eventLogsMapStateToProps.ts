import { connect, ConnectedProps } from 'react-redux';
import { MapStateToProps, Dispatch } from '../../../../../types';
import { RouterTriggerProps } from '../../types';
import { NotFoundError } from '../../../../Error/PageNotFound';

type PropsFromState = {
  triggerName: string;
  readOnlyMode: boolean;
};

const mapStateToProps: MapStateToProps<PropsFromState, RouterTriggerProps> = (
  state,
  ownProps
) => {
  const triggerList = state.events.triggers.event;
  const triggerName = ownProps.params.triggerName;

  const currentTrigger = triggerList.find(tr => tr.name === triggerName);

  if (!currentTrigger) {
    // throw a 404 exception
    throw new NotFoundError();
  }

  return {
    triggerName,
    readOnlyMode: state.main.readOnlyMode,
  };
};

const mapDispatchToProps = (dispatch: Dispatch) => ({ dispatch });

export const connector = connect(mapStateToProps, mapDispatchToProps);

export type EventsLogsInjectedProps = ConnectedProps<typeof connector>;
