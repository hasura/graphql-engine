import React from 'react';
import { connect, ConnectedProps } from 'react-redux';
import STContainer from '../Container';
import Modify from './Modify';
import { mapDispatchToPropsEmpty } from '../../../../Common/utils/reactUtils';
import { ReduxState } from '../../../../../types';
import { useGetCronTriggers } from '../Hooks/useGetCronTriggers';

type Props = InjectedProps;

const ModifyContainer: React.FC<Props> = props => {
  const { data: cronTriggers, isLoading, error } = useGetCronTriggers();
  if (isLoading) {
    return <span>Loading...</span>;
  }
  if (!cronTriggers) {
    return <span>Could not find any cron triggers</span>;
  }
  if (error) {
    return <span>There was an error, please try again later</span>;
  }
  const { dispatch, triggerName, readOnlyMode, eventsLoading } = props;
  return (
    <STContainer
      tabName="modify"
      dispatch={dispatch}
      triggerName={triggerName}
      allTriggers={cronTriggers}
      eventsLoading={eventsLoading}
    >
      {readOnlyMode ? 'Cannot modify in read-only mode' : <Modify />}
    </STContainer>
  );
};

const mapStateToProps = (
  state: ReduxState,
  ownProps: { params: { triggerName: string } }
) => {
  return {
    readOnlyMode: state.main.readOnlyMode,
    triggerName: ownProps.params.triggerName,
    eventsLoading: state.events.loading,
  };
};

const connector = connect(mapStateToProps, mapDispatchToPropsEmpty);

type InjectedProps = ConnectedProps<typeof connector>;

const ModifyConnector = connector(ModifyContainer);
export default ModifyConnector;
