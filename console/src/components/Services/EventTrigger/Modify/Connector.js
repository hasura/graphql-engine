import Modify from './Modify';

const mapStateToProps = (state, ownProps) => {
  return {
    modifyTrigger: state.modifyTrigger,
    modifyTriggerName: ownProps.params.trigger,
    triggerList: state.triggers.triggerList,
    schemaList: state.tables.schemaList,
    allSchemas: state.tables.allSchemas,
    serverVersion: state.main.serverVersion,
    currentSchema: state.tables.currentSchema,
  };
};

const modifyTriggerConnector = connect => connect(mapStateToProps)(Modify);

export default modifyTriggerConnector;
