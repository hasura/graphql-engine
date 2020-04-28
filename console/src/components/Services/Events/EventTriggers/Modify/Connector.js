import Modify from './Modify';

const mapStateToProps = (state, ownProps) => {
  console.log(state);

  const triggerList = state.events.new.triggers.event;
  const modifyTriggerName = ownProps.params.trigger;

  const currentTrigger = triggerList.find(tr => tr.name === modifyTriggerName);

  return {
    modifyTrigger: state.events.legacy.modifyTrigger,
    triggerList,
    currentTrigger,
    schemaList: state.tables.schemaList,
    allSchemas: state.tables.allSchemas,
    serverVersion: state.main.serverVersion,
    currentSchema: state.tables.currentSchema,
    readOnlyMode: state.main.readOnlyMode,
  };
};

const modifyTriggerConnector = connect => connect(mapStateToProps)(Modify);

export default modifyTriggerConnector;
