import Modify from './Modify';
import { connect } from 'react-redux';

const mapStateToProps = (state, ownProps) => {
  return {
    modifyTrigger: state.modifyTrigger,
    modifyTriggerName: ownProps.params.trigger,
    triggerList: state.triggers.triggerList,
    schemaList: state.tables.schemaList,
    allSchemas: state.tables.allSchemas,
    serverVersion: state.main.serverVersion,
    currentSchema: state.tables.currentSchema,
    readOnlyMode: state.main.readOnlyMode,
  };
};

const ConnectedModifyTrigger = connect(mapStateToProps)(Modify);
export default ConnectedModifyTrigger;
