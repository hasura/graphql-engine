import Relationships from './Main';
import { connect } from 'react-redux';

const mapStateToProps = state => {
  return {
    ...state.actions.relationships,
    allActions: state.actions.common.actions,
    allTables: state.tables.allSchemas,
    schemaList: state.tables.schemaList,
    allTypes: state.types.types,
  };
};

const ConnectedActionRelationships = connect(mapStateToProps)(Relationships);
export default ConnectedActionRelationships;
