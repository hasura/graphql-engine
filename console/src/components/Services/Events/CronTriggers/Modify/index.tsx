import React from 'react';
import { connect, ConnectedProps } from 'react-redux';
import STContainer from '../Container';
import Modify from './Modify';
import { mapDispatchToPropsEmpty } from '../../../../Common/utils/reactUtils';
import { ReduxState } from '../../../../../types';
import { getCronTriggers } from '../../../../../metadata/selector';

interface Props extends InjectedProps {}

const ModifyContainer: React.FC<Props> = props => {
  const {
    dispatch,
    cronTriggers,
    triggerName,
    readOnlyMode,
    eventsLoading,
  } = props;
  return (
    <STContainer
      tabName="modify"
      dispatch={dispatch}
      triggerName={triggerName}
      allTriggers={cronTriggers}
      eventsLoading={eventsLoading}
    >
      {readOnlyMode ? (
        'Cannot modify in read-only mode'
      ) : (
        <Modify dispatch={dispatch} />
      )}
    </STContainer>
  );
};

const mapStateToProps = (
  state: ReduxState,
  ownProps: { params: { triggerName: string } }
) => {
  return {
    cronTriggers: getCronTriggers(state),
    readOnlyMode: state.main.readOnlyMode,
    triggerName: ownProps.params.triggerName,
    eventsLoading: state.events.loading,
  };
};

const connector = connect(mapStateToProps, mapDispatchToPropsEmpty);

type InjectedProps = ConnectedProps<typeof connector>;

const ModifyConnector = connector(ModifyContainer);
export default ModifyConnector;
