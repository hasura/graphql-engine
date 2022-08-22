import React from 'react';
import { connect, ConnectedProps } from 'react-redux';
import { mapDispatchToPropsEmpty } from '../../../../Common/utils/reactUtils';
import AdhocEventsContainer from '../Container';
import Logs from './Logs';

interface LogsProps extends InjectedProps {}

const LogsContainer = ({ dispatch }: LogsProps) => {
  return (
    <AdhocEventsContainer tabName="logs" dispatch={dispatch}>
      <Logs dispatch={dispatch} />
    </AdhocEventsContainer>
  );
};

const connector = connect(null, mapDispatchToPropsEmpty);

type InjectedProps = ConnectedProps<typeof connector>;

const LogsConnector = connector(LogsContainer);
export default LogsConnector;
