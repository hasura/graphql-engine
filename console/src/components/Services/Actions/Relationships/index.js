import Relationships from './Main';
import { actionsSelector, customTypesSelector } from '../selectors';

const mapStateToProps = state => {
  return {
    ...state.actions.relationships,
    allActions: actionsSelector(state),
    allTables: state.tables.allSchemas,
    schemaList: state.tables.schemaList,
    allTypes: customTypesSelector(state),
    readOnlyMode: state.main.readOnlyMode,
  };
};

const connector = connect => connect(mapStateToProps)(Relationships);
export default connector;
