import React from 'react';
import { connect, ConnectedProps } from 'react-redux';
import STContainer from '../Container';
import { Triggers, RouterTriggerProps } from '../../types';
import { MapStateToProps } from '../../../../../types';
import Logs from './Logs';
import { mapDispatchToPropsEmpty } from '../../../../Common/utils/reactUtils';
import { getCronTriggers } from '../../../../../metadata/selector';

interface Props extends InjectedProps {}

const LogsContainer: React.FC<Props> = ({
  dispatch,
  allTriggers,
  triggerName,
}) => {
  return (
    <STContainer
      tabName="logs"
      dispatch={dispatch}
      triggerName={triggerName}
      allTriggers={allTriggers}
    >
      <Logs dispatch={dispatch} />
    </STContainer>
  );
};

type PropsFromState = {
  allTriggers: Triggers;
  triggerName: string;
};

const mapStateToProps: MapStateToProps<PropsFromState, RouterTriggerProps> = (
  state,
  ownProps
) => {
  return {
    allTriggers: getCronTriggers(state),
    triggerName: ownProps.params.triggerName,
  };
};

const connector = connect(mapStateToProps, mapDispatchToPropsEmpty);

type InjectedProps = ConnectedProps<typeof connector>;

const LogsConnector = connector(LogsContainer);
export default LogsConnector;
