import React from 'react';
import { connect, ConnectedProps } from 'react-redux';
import { mapDispatchToPropsEmpty } from '../../../../Common/utils/reactUtils';
import AdhocEventsContainer from '../Container';
import Add from './Add';

interface Props extends InjectedProps {}

const AddContainer: React.FC<Props> = ({ dispatch }) => {
  return (
    <AdhocEventsContainer tabName="add" dispatch={dispatch}>
      <Add dispatch={dispatch} />
    </AdhocEventsContainer>
  );
};

const connector = connect(null, mapDispatchToPropsEmpty);

type InjectedProps = ConnectedProps<typeof connector>;

export default connector(AddContainer);
