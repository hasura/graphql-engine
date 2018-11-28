import React from 'react';
import { AddTrigger } from '../Add/AddTrigger';

const mapStateToProps = (state, ownProps) => {
  return {
    ...state.addTrigger,
    modifyTriggerName: ownProps.params.trigger,
    triggerList: state.triggers.triggerList,
    schemaList: state.tables.schemaList,
    allSchemas: state.tables.allSchemas,
    serverVersion: state.main.serverVersion ? state.main.serverVersion : '',
  };
};

const ModifyTrigger = props => <AddTrigger {...props} modify />;

const modifyTriggerConnector = connect =>
  connect(mapStateToProps)(ModifyTrigger);

export default modifyTriggerConnector;
