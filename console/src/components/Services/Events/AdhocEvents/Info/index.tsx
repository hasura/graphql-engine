import React from 'react';
import { connect, ConnectedProps } from 'react-redux';
import { mapDispatchToPropsEmpty } from '../../../../Common/utils/reactUtils';
import AdhocEventsContainer from '../Container';
import Info from './Info';

interface Props extends InjectedProps {}

const InfoContainer: React.FC<Props> = ({ dispatch }) => {
  return (
    <AdhocEventsContainer tabName="info" dispatch={dispatch}>
      <Info />
    </AdhocEventsContainer>
  );
};

const connector = connect(null, mapDispatchToPropsEmpty);

type InjectedProps = ConnectedProps<typeof connector>;

const InfoConnector = connector(InfoContainer);

export default InfoConnector;
