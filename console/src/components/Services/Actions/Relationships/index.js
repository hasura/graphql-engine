import Relationships from './Main';

const mapStateToProps = state => {
  return {
    ...state.actions.relationships,
    allActions: state.actions.common.actions,
    allTables: state.tables.allSchemas,
    schemaList: state.tables.schemaList,
    allTypes: state.types.types,
  };
};

const connector = connect => connect(mapStateToProps)(Relationships);
export default connector;
